CLASS z_sy_cl_featurelist_rest DEFINITION
  PUBLIC
  INHERITING FROM cl_rest_resource
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    CONSTANTS class_name TYPE seoclsname VALUE `Z_SY_CL_FEATURELIST_REST` ##NO_TEXT.
    CONSTANTS c_devclass TYPE zeben-devclass VALUE 'Z_SY_FEATURETOGGLE' ##NO_TEXT.
    CONSTANTS c_repname_put TYPE zeben-name VALUE 'PUT_TOGGLES' ##NO_TEXT.

    METHODS if_rest_resource~get REDEFINITION .
    METHODS if_rest_resource~put REDEFINITION .

  PROTECTED SECTION.

    METHODS check_write_auth RETURNING VALUE(rv_auth) TYPE abap_bool .
  PRIVATE SECTION.

    TYPES:
      BEGIN OF featurestate,
        class  TYPE z_sy_featureclassname,
        name   TYPE z_sy_featurename,
        active TYPE xsdboolean,
      END OF featurestate .
    TYPES:
      t_featurestate TYPE STANDARD TABLE OF featurestate WITH NON-UNIQUE KEY name .
ENDCLASS.



CLASS z_sy_cl_featurelist_rest IMPLEMENTATION.


  METHOD check_write_auth.

    "implement authority check here
    rv_auth = abap_true.

  ENDMETHOD.


  METHOD if_rest_resource~get.

    DATA(featurelist) = NEW z_sy_cl_feature_infrastruct( )->getfeaturedescriptions( ).

    DATA(writer) = cl_sxml_string_writer=>create( type = if_sxml=>co_xt_json ).
    CALL TRANSFORMATION id SOURCE data = featurelist RESULT XML writer.
    DATA(response_entity) = mo_response->create_entity( ).
    response_entity->set_binary_data( writer->get_output( ) ).

  ENDMETHOD.


  METHOD if_rest_resource~put.

    IF check_write_auth( ) = abap_false.
      RAISE EXCEPTION TYPE cx_rest_exception EXPORTING status_code = cl_rest_status_code=>gc_client_error_unauthorized.
    ENDIF.

    DATA(message) = io_entity->get_binary_data( ).

    DATA featurelist TYPE t_featurestate.
    CALL TRANSFORMATION id SOURCE XML message RESULT data = featurelist.

    DATA(infrastructure) = NEW z_sy_cl_feature_infrastruct( ).
    LOOP AT featurelist ASSIGNING FIELD-SYMBOL(<feature>).
      infrastructure->setfeaturestate( i_feature = VALUE #( class = <feature>-class name = <feature>-name ) i_newstate = <feature>-active i_activaterecursive = abap_true ).
    ENDLOOP.

  ENDMETHOD.
ENDCLASS.
