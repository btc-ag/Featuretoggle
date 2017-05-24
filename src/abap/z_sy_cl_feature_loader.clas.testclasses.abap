class lcl_feature_loader definition create public inheriting from Z_sy_cl_feature_loader
.


  public section.
   methods constructor importing i_feature_instance type ref to Z_sy_feature.
   class-methods set_feature_state
     importing
      i_featurename type Z_sy_featurename
      i_active type abap_bool.
   methods loadft.

  protected section.
  methods loadtogglestate redefinition.
  methods getfeaturelist redefinition.
  private section.
   types: begin of feature_state,
          name type Z_sy_featurename,
          value type abap_bool,
          end of feature_state.
   class-data feature_states type hashed table of feature_state with unique key name.

endclass.

class lcl_feature_loader implementation.

  METHOD loadtogglestate.
      try.
         r_state = feature_states[ name = i_featuretoggle ]-value.
      catch cx_sy_itab_line_not_found.
         r_state = abap_false.
      endtry.
  ENDMETHOD.

  METHOD constructor.
    super->constructor( i_feature_instance ).
  ENDMETHOD.

  METHOD set_feature_state.
    insert value #( name = i_featurename value = i_active ) into table feature_states.
    if sy-subrc = 4.
       read table feature_states with table key name = i_featurename assigning field-symbol(<feature>).
       <feature>-value = i_active.
    endif.
  ENDMETHOD.

  METHOD getfeaturelist.
    r_list = value #( ( 'F1' ) ( 'F2' ) ( 'F3' ) ( 'F4' ) ).
  ENDMETHOD.

  METHOD loadft.
    loadfeaturetoggles( ).
  ENDMETHOD.

endclass.

class lcl_feature definition
  inheriting from Z_SY_FEATURE
  final
  create public
  friends Z_sy_cl_feature_loader.

  public section.
    class-data F1 type ABAP_BOOL read-only .
    data F1_DESC type STRING value `F1 Description.` ##NO_TEXT.
    data F1_DEP type STRING value `F2` ##NO_TEXT.

    class-data F2 type ABAP_BOOL read-only .
    data F2_DESC type STRING value `F2 Description.` ##NO_TEXT.
    data F2_DEP type STRING value `F1` ##NO_TEXT.

    class-data F3 type ABAP_BOOL read-only .
    data F3_DESC type STRING value `F3 Description.` ##NO_TEXT.
    data F3_DEP type STRING value `F1` ##NO_TEXT.

    class-data F4 type ABAP_BOOL read-only .
    data F4_DESC type STRING value `F4 Description.` ##NO_TEXT.

  protected section.
  private section.

endclass.

class lcl_feature implementation.


endclass.

class ltcl_feature_loader definition final for testing
  duration short
  risk level harmless.

  private section.
    methods load_states1 for testing raising cx_static_check.
    methods load_states2 for testing raising cx_static_check.
endclass.


class ltcl_feature_loader implementation.

  method load_states1.
    lcl_feature_loader=>set_feature_state( i_featurename = 'F1' i_active = abap_true ).
    lcl_feature_loader=>set_feature_state( i_featurename = 'F2' i_active = abap_true ).
    lcl_feature_loader=>set_feature_state( i_featurename = 'F3' i_active = abap_false ).
    lcl_feature_loader=>set_feature_state( i_featurename = 'F4' i_active = abap_false ).

    new lcl_feature_loader( new lcl_feature( ) )->loadft( ).

    cl_abap_unit_assert=>assert_equals( msg = 'F1 not active' exp = abap_true act = lcl_feature=>f1 ).
    cl_abap_unit_assert=>assert_equals( msg = 'F2 not active' exp = abap_true act = lcl_feature=>f2 ).
    cl_abap_unit_assert=>assert_equals( msg = 'F3 active' exp = abap_false act = lcl_feature=>f3 ).
    cl_abap_unit_assert=>assert_equals( msg = 'F4 active' exp = abap_false act = lcl_feature=>f4 ).
  endmethod.

  method load_states2.
    lcl_feature_loader=>set_feature_state( i_featurename = 'F1' i_active = abap_false ).
    lcl_feature_loader=>set_feature_state( i_featurename = 'F2' i_active = abap_false ).
    lcl_feature_loader=>set_feature_state( i_featurename = 'F3' i_active = abap_false ).
    lcl_feature_loader=>set_feature_state( i_featurename = 'F4' i_active = abap_true ).

    new lcl_feature_loader( new lcl_feature( ) )->loadft( ).

    cl_abap_unit_assert=>assert_equals( msg = 'F1 active' exp = abap_false act = lcl_feature=>f1 ).
    cl_abap_unit_assert=>assert_equals( msg = 'F2 active' exp = abap_false act = lcl_feature=>f2 ).
    cl_abap_unit_assert=>assert_equals( msg = 'F3 active' exp = abap_false act = lcl_feature=>f3 ).
    cl_abap_unit_assert=>assert_equals( msg = 'F4 not active' exp = abap_true act = lcl_feature=>f4 ).
  endmethod.

endclass.
