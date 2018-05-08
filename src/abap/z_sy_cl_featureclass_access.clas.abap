CLASS z_sy_cl_featureclass_access DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS constructor
      IMPORTING
        !i_classname TYPE z_sy_featureclassname .
    METHODS getfeaturelist
      RETURNING
        VALUE(r_list) TYPE z_sy_t_featurename .
    METHODS getfeaturestate
      IMPORTING
        !i_featurename  TYPE z_sy_featurename
      RETURNING
        VALUE(r_active) TYPE abap_bool .
    METHODS getfeaturedate
      IMPORTING
        !i_featurename TYPE z_sy_featurename
      RETURNING
        VALUE(r_date)  TYPE z_sy_featuredate .
    METHODS getfeaturedescription
      IMPORTING
        !i_featurename       TYPE z_sy_featurename
      RETURNING
        VALUE(r_description) TYPE string .
    METHODS getfeaturedependencies
      IMPORTING
        !i_featurename        TYPE z_sy_featurename
      RETURNING
        VALUE(r_dependencies) TYPE z_sy_t_featurereference .
  PROTECTED SECTION.
  PRIVATE SECTION.

    DATA classname TYPE z_sy_featureclassname .
    DATA feature_instance TYPE REF TO z_sy_feature .
ENDCLASS.



CLASS z_sy_cl_featureclass_access IMPLEMENTATION.


  METHOD constructor.

    me->classname = i_classname.
    "create object feature_instance type (me->classname).
    me->feature_instance = z_sy_cl_feature_loader=>createfeaturetogglesinstance( me->classname ).

  ENDMETHOD.


  METHOD getfeaturedate.

    DATA(attributename) = |{ i_featurename }_DATE|.
    FIELD-SYMBOLS <attributeaccess> TYPE z_sy_featuredate.
    ASSIGN feature_instance->(attributename) TO <attributeaccess>.
    IF sy-subrc = 0.
      r_date = <attributeaccess>.
    ENDIF.

  ENDMETHOD.


  METHOD getfeaturedependencies.

    DATA(attributename) = |{ i_featurename }_DEP|.
    FIELD-SYMBOLS <attributeaccess> TYPE string.
    ASSIGN feature_instance->(attributename) TO <attributeaccess>.
    IF sy-subrc = 0.
      TRANSLATE <attributeaccess> TO UPPER CASE.
      SPLIT <attributeaccess> AT `,` INTO TABLE DATA(dependencies).
      LOOP AT dependencies ASSIGNING FIELD-SYMBOL(<dep>).
        SHIFT <dep> LEFT DELETING LEADING space.
        SPLIT <dep> AT `=>` INTO DATA(class) DATA(attribute).
        IF attribute IS INITIAL.
          attribute = class.
          class = me->classname.
        ENDIF.
        INSERT VALUE #( class = class name = attribute ) INTO TABLE r_dependencies.
      ENDLOOP.
    ENDIF.

  ENDMETHOD.


  METHOD getfeaturedescription.

    DATA(attributename) = |{ i_featurename }_DESC|.
    FIELD-SYMBOLS <attributeaccess> TYPE string.
    ASSIGN feature_instance->(attributename) TO <attributeaccess>.
    IF sy-subrc = 0.
      r_description = <attributeaccess>.
    ENDIF.

  ENDMETHOD.


  METHOD getfeaturelist.

    DATA(object_descriptor) = CAST cl_abap_objectdescr( cl_abap_typedescr=>describe_by_object_ref( feature_instance ) ).
    r_list = VALUE #( FOR <attribute> IN object_descriptor->attributes
                      WHERE ( is_class = abap_true AND visibility = cl_abap_objectdescr=>public AND type_kind = cl_abap_typedescr=>typekind_char AND length <= 2 )
                      ( <attribute>-name ) ).

  ENDMETHOD.


  METHOD getfeaturestate.

    FIELD-SYMBOLS <attribute_access> TYPE abap_bool.
    ASSIGN (classname)=>(i_featurename) TO <attribute_access>.
    r_active = <attribute_access>.

  ENDMETHOD.
ENDCLASS.
