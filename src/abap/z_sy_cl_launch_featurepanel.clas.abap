CLASS z_sy_cl_launch_featurepanel DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS launch_ui5_app .
  PROTECTED SECTION.

    DATA url TYPE string VALUE `/sap/bc/ui5_ui5/sap/z_sy_feature` ##NO_TEXT.
  PRIVATE SECTION.

    METHODS get_server_url
      RETURNING
        VALUE(r_result) TYPE char1024 .
ENDCLASS.



CLASS z_sy_cl_launch_featurepanel IMPLEMENTATION.


  METHOD get_server_url.
    r_result = |{ cl_http_server=>get_location( ) }{ me->url }|.
  ENDMETHOD.


  METHOD launch_ui5_app.
    DATA(url) = get_server_url( ).
    TRY.
        DATA empty_co TYPE REF TO cl_gui_container.         "#EC NEEDED
        DATA(html_viewer) = NEW cl_gui_html_viewer( parent = empty_co ).
        html_viewer->enable_sapsso( enabled = abap_true ).
        html_viewer->detach_url_in_browser( url ).
        cl_gui_cfw=>flush( ).
      CATCH cx_root.
        CALL FUNCTION 'CALL_BROWSER'
          EXPORTING
            url                    = url
          EXCEPTIONS
            frontend_not_supported = 1
            frontend_error         = 2
            prog_not_found         = 3
            no_batch               = 4
            unspecified_error      = 5.
        IF sy-subrc NE 0.
          MESSAGE e001(00) WITH 'Cannot start browser'(108).
        ENDIF.
    ENDTRY.
  ENDMETHOD.
ENDCLASS.
