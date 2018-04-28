REPORT z_sy_del_features MESSAGE-ID  z_sy
                LINE-SIZE   128
                LINE-COUNT  50.

************************************************************************
* Autor      : Volker Wohlers, BTC AG
* Datum      : 11.01.2016
* Änderungen : Zeichen/Datum     Art
*              VOWOHLER/11.01.16 Erstellung (EAE)
************************************************************************
*
* Dieses Programm löscht alle Schalter. Dies wird im Normalfall im
* QS-System vor einer Produktivsetzung ausgeführt und soll die
* Simulation für das Produktivsystem abbilden.
*
************************************************************************

*----------------------------------------------------------------------*
*        I N T E R N E  D A T E N F E L D E R                          *
*----------------------------------------------------------------------*

DATA: gt_data        TYPE STANDARD TABLE OF zsy_tfeature.
DATA: gs_data        TYPE zsy_tfeature.

*----------------------------------------------------------------------*
*        A U S W A H L K R I T E R I E N  SELEKTIONSBILD FESTLEGEN     *
*----------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK w_block_001
                 WITH FRAME TITLE TEXT-001.

SELECT-OPTIONS: so_cla    FOR gs_data-class.
SELECT-OPTIONS: so_fea    FOR gs_data-feature.
SELECT-OPTIONS: so_act    FOR gs_data-active.

SELECTION-SCREEN END   OF BLOCK w_block_001.


SELECTION-SCREEN BEGIN OF BLOCK w_block_002
                 WITH FRAME TITLE TEXT-002.

PARAMETERS: pa_show RADIOBUTTON GROUP cntr DEFAULT 'X',
            pa_dele RADIOBUTTON GROUP cntr.

SELECTION-SCREEN END   OF BLOCK w_block_002.

*----------------------------------------------------------------------*
*        V E R A R B E I T U N G                                       *
*----------------------------------------------------------------------*

START-OF-SELECTION.

  PERFORM daten_selektion.

  IF pa_show = 'X'.
    PERFORM daten_anzeigen.
  ELSE.
    PERFORM daten_loeschen.
  ENDIF.

*&---------------------------------------------------------------------*
*&      Form  DATEN_SELEKTION
*&---------------------------------------------------------------------*
FORM daten_selektion .

  SELECT * INTO TABLE gt_data
    FROM zsy_tfeature
    WHERE class   IN so_cla
      AND feature IN so_fea
      AND active  IN so_act.

  IF sy-subrc <> 0.
    REFRESH gt_data.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  DATEN_ANZEIGEN
*&---------------------------------------------------------------------*
FORM daten_anzeigen .
  DATA: lr_alv   TYPE REF TO cl_salv_table.

* Ausgabe der Daten über ALV
  TRY.
      cl_salv_table=>factory(
        IMPORTING
          r_salv_table   = lr_alv    " Basis Class Simple ALV Tables
        CHANGING
          t_table        = gt_data
      ).
    CATCH cx_salv_msg.    "
      ASSERT 1 = 2.
  ENDTRY.


  lr_alv->display( ).

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  DATEN_LOESCHEN
*&---------------------------------------------------------------------*
FORM daten_loeschen .

  FIELD-SYMBOLS: <ls_data>   LIKE gs_data.

  LOOP AT gt_data ASSIGNING <ls_data>.
    WRITE: / 'Schalter', <ls_data>-feature,
             'Aktiv-Kz.:', <ls_data>-active, ' wird gelöscht!'.
  ENDLOOP.

  DELETE zsy_tfeature FROM TABLE gt_data.

  IF sy-subrc = 0.
    WRITE: / 'Schalter wurden gelöscht!'.
    COMMIT WORK.
  ELSE.
    WRITE: / 'Fehler beim Löschen der Daten, bitte prüfen!'.
    ROLLBACK WORK.
  ENDIF.

ENDFORM.
