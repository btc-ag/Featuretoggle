CLASS z_sy_cl_feature_webservice DEFINITION
  PUBLIC
  INHERITING FROM cl_rest_http_handler
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS if_rest_application~get_root_handler REDEFINITION .
  PROTECTED SECTION.

    METHODS handle_csrf_token REDEFINITION .
  PRIVATE SECTION.
ENDCLASS.



CLASS z_sy_cl_feature_webservice IMPLEMENTATION.

  METHOD handle_csrf_token.
    "@ToDO implement csrf logic.
  ENDMETHOD.


  METHOD if_rest_application~get_root_handler.

    DATA(router) = NEW cl_rest_router( ).
    router->attach( iv_template = `/featurelist.json` iv_handler_class = z_sy_cl_featurelist_rest=>class_name ).
    ro_root_handler = router.

  ENDMETHOD.

ENDCLASS.
