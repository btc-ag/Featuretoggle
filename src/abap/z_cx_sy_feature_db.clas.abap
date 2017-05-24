CLASS z_cx_sy_feature_db DEFINITION
  PUBLIC
  INHERITING FROM cx_dynamic_check
  CREATE PUBLIC .

  PUBLIC SECTION.

    CONSTANTS z_cx_sy_feature_db TYPE sotr_conc VALUE '52540A0101621EE78DB20D275EF3A5BD' ##NO_TEXT.
    CONSTANTS invalid_featurename TYPE sotr_conc VALUE '52540A0101621EE78DB20D275EF3C5BD' ##NO_TEXT.
    CONSTANTS existing_dependencies TYPE sotr_conc VALUE '52540A0101DC1ED593FBD994A9A74D4A' ##NO_TEXT.

    METHODS constructor
      IMPORTING
        !textid   LIKE textid OPTIONAL
        !previous LIKE previous OPTIONAL .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS z_cx_sy_feature_db IMPLEMENTATION.


  METHOD constructor ##ADT_SUPPRESS_GENERATION.
    CALL METHOD super->constructor
      EXPORTING
        textid   = textid
        previous = previous.
    IF textid IS INITIAL.
      me->textid = z_cx_sy_feature_db .
    ENDIF.
  ENDMETHOD.
ENDCLASS.
