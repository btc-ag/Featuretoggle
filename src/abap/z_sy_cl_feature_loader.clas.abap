CLASS z_sy_cl_feature_loader DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS constructor
      IMPORTING
        !i_feature_instance TYPE REF TO z_sy_feature .
    CLASS-METHODS load
      IMPORTING
        !i_feature_instance TYPE REF TO z_sy_feature .
    CLASS-METHODS createfeaturetogglesinstance
      IMPORTING
        !i_featureclass   TYPE z_sy_featureclassname
      RETURNING
        VALUE(r_instance) TYPE REF TO z_sy_feature .
  PROTECTED SECTION.

    METHODS loadtogglestate
      IMPORTING
        !i_featuretoggle TYPE abap_attrname
      RETURNING
        VALUE(r_state)   TYPE abap_bool .
    METHODS getfeaturelist
      RETURNING
        VALUE(r_list) TYPE z_sy_t_featurename .
    METHODS loadfeaturetoggles .
  PRIVATE SECTION.

    DATA feature_instance TYPE REF TO z_sy_feature .
    DATA classname TYPE z_sy_featureclassname .

    METHODS getfeatureclassname
      RETURNING
        VALUE(r_name) TYPE string .
ENDCLASS.



CLASS z_sy_cl_feature_loader IMPLEMENTATION.


  METHOD constructor.

    feature_instance = i_feature_instance.
    me->classname = cl_abap_typedescr=>describe_by_object_ref( feature_instance )->get_relative_name( ).

  ENDMETHOD.


  METHOD createfeaturetogglesinstance.

    CREATE OBJECT r_instance TYPE (i_featureclass).

  ENDMETHOD.


  METHOD getfeatureclassname.

    DATA(object_descriptor) = CAST cl_abap_objectdescr( cl_abap_typedescr=>describe_by_object_ref( feature_instance ) ).
    r_name = object_descriptor->get_relative_name( ).

  ENDMETHOD.


  METHOD getfeaturelist.

    DATA(object_descriptor) = CAST cl_abap_objectdescr( cl_abap_typedescr=>describe_by_object_ref( feature_instance ) ).
    r_list = VALUE #( FOR <attribute> IN object_descriptor->attributes
                      WHERE ( is_class = abap_true AND visibility = cl_abap_objectdescr=>public AND type_kind = cl_abap_typedescr=>typekind_char AND length <= 2 )
                      ( <attribute>-name ) ).

  ENDMETHOD.


  METHOD load.

    NEW z_sy_cl_feature_loader( i_feature_instance )->loadfeaturetoggles( ).

  ENDMETHOD.


  METHOD loadfeaturetoggles.

    TRY.
        DATA(classname) = getfeatureclassname( ).
        LOOP AT getfeaturelist( ) ASSIGNING FIELD-SYMBOL(<attributename>).
          FIELD-SYMBOLS <attribute_access> TYPE abap_bool.
          ASSIGN (classname)=>(<attributename>) TO <attribute_access>.
          <attribute_access> = loadtogglestate( <attributename> ).
        ENDLOOP.
      CATCH cx_root.
        "all features are disabled.
    ENDTRY.

  ENDMETHOD.


  METHOD loadtogglestate.

    SELECT SINGLE active INTO @r_state FROM zsy_tfeature WHERE class = @me->classname AND feature = @i_featuretoggle. "table is buffered.
    IF sy-subrc <> 0.
      CLEAR r_state.
    ENDIF.

  ENDMETHOD.
ENDCLASS.
