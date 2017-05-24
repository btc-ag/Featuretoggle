CLASS z_sy_cl_feature_infrastruct DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    TYPES:
      BEGIN OF featuredescription,
        class        TYPE z_sy_featureclassname,
        name         TYPE z_sy_featurename,
        description  TYPE string,
        dependencies TYPE z_sy_t_featurereference,
        currentstate TYPE xsdboolean,
        date         TYPE date,
      END OF featuredescription .
    TYPES:
      t_featuredescription TYPE STANDARD TABLE OF featuredescription WITH KEY class name .

    METHODS getfeaturedescriptions RETURNING VALUE(r_result) TYPE t_featuredescription .
    METHODS constructor .
    METHODS setfeaturestate
      IMPORTING
        !i_feature           TYPE zsy_featurereference
        !i_newstate          TYPE abap_bool
        !i_activaterecursive TYPE abap_bool .
  PROTECTED SECTION.
  PRIVATE SECTION.

    DATA featurelist TYPE t_featuredescription .

    METHODS reflect_feature_toggles .
    METHODS resolvefeaturedependencies
      IMPORTING
        !i_feature            TYPE zsy_featurereference
      CHANGING
        !x_dependencies       TYPE z_sy_t_featurereference OPTIONAL
      RETURNING
        VALUE(r_dependencies) TYPE z_sy_t_featurereference .
    METHODS getdependentfeatures
      IMPORTING
        !i_feature          TYPE featuredescription
      RETURNING
        VALUE(r_dependents) TYPE z_sy_t_featurereference .
    METHODS writefeaturestate
      IMPORTING
        !i_feature  TYPE zsy_featurereference
        !i_newstate TYPE abap_bool
      RAISING
        z_cx_sy_feature_db .
ENDCLASS.



CLASS z_sy_cl_feature_infrastruct IMPLEMENTATION.


  METHOD constructor.

    me->reflect_feature_toggles( ).

  ENDMETHOD.


  METHOD getdependentfeatures.

    LOOP AT featurelist ASSIGNING FIELD-SYMBOL(<feature>).
      IF line_exists( <feature>-dependencies[ class = i_feature-class name = i_feature-name ] ).
        INSERT VALUE #( class = <feature>-class name = <feature>-name ) INTO TABLE r_dependents.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.


  METHOD getfeaturedescriptions.

    r_result = featurelist.

  ENDMETHOD.


  METHOD reflect_feature_toggles.

    DATA subclasses TYPE seor_inheritance_keys.
    CALL FUNCTION 'SEO_CLASS_GET_ALL_SUBS'
      EXPORTING
        clskey             = CONV seoclskey( 'Z_SY_FEATURE' )
        version            = seoc_version_active
      IMPORTING
        inhkeys            = subclasses
      EXCEPTIONS
        class_not_existing = 1
        OTHERS             = 2.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE z_cx_sy_feature_db.
    ENDIF.
    CLEAR me->featurelist.
    LOOP AT subclasses ASSIGNING FIELD-SYMBOL(<class>).
      DATA(class_access) = NEW z_sy_cl_featureclass_access( CONV #( <class>-clsname ) ).
      DATA(features) = VALUE t_featuredescription( FOR <attribute> IN class_access->getfeaturelist( )
                    (
                       class = <class>-clsname
                       name = <attribute>
                       currentstate = class_access->getfeaturestate( <attribute> )
                       description = class_access->getfeaturedescription( <attribute> )
                       dependencies = class_access->getfeaturedependencies( <attribute> )
                       date = class_access->getfeaturedate( <attribute> )
                    ) ).
      INSERT LINES OF features INTO TABLE me->featurelist.
    ENDLOOP.
    LOOP AT me->featurelist ASSIGNING FIELD-SYMBOL(<feature>).
      <feature>-dependencies = resolvefeaturedependencies( VALUE #( class = <feature>-class name = <feature>-name ) ).
      DELETE <feature>-dependencies WHERE class = <feature>-class AND name = <feature>-name.
    ENDLOOP.
    SORT me->featurelist BY class name ASCENDING.

  ENDMETHOD.


  METHOD resolvefeaturedependencies.

    TRY.
        DATA(feature) = me->featurelist[ class = i_feature-class name = i_feature-name ].
        LOOP AT feature-dependencies ASSIGNING FIELD-SYMBOL(<dependency>).
          IF line_exists( x_dependencies[ class = <dependency>-class name = <dependency>-name ] ).
            CONTINUE.
          ENDIF.
          INSERT <dependency> INTO TABLE x_dependencies.
          INSERT <dependency> INTO TABLE r_dependencies.
          INSERT LINES OF resolvefeaturedependencies( EXPORTING i_feature = <dependency> CHANGING x_dependencies = x_dependencies ) INTO TABLE r_dependencies.
        ENDLOOP.
      CATCH cx_sy_itab_line_not_found.
    ENDTRY.

  ENDMETHOD.


  METHOD setfeaturestate.

    TRY.
        DATA(feature) = featurelist[ class = i_feature-class name = i_feature-name ].
      CATCH cx_sy_itab_line_not_found.
        RAISE EXCEPTION TYPE z_cx_sy_feature_db EXPORTING textid = z_cx_sy_feature_db=>invalid_featurename.
    ENDTRY.

    "Abhaengige Faetures mit aktivieren.
    IF i_newstate = abap_true.
      DATA(dependencies) = feature-dependencies.
    ELSE.
      dependencies = me->getdependentfeatures( feature ).
    ENDIF.
    IF lines( dependencies ) > 0.
      IF i_activaterecursive = abap_false.
        RAISE EXCEPTION TYPE z_cx_sy_feature_db EXPORTING textid = z_cx_sy_feature_db=>existing_dependencies.
      ENDIF.
      LOOP AT dependencies ASSIGNING FIELD-SYMBOL(<dependency>).
        writefeaturestate( i_feature = <dependency> i_newstate = i_newstate ).
      ENDLOOP.
    ENDIF.


    writefeaturestate( i_feature = i_feature i_newstate = i_newstate ).

  ENDMETHOD.


  METHOD writefeaturestate.

    DATA(record) = VALUE zsy_tfeature( class = i_feature-class feature = i_feature-name active = i_newstate ).
    UPDATE zsy_tfeature FROM record.
    IF sy-subrc <> 0.
      INSERT zsy_tfeature FROM record.
      IF sy-subrc <> 0.
        RAISE EXCEPTION TYPE z_cx_sy_feature_db.
      ENDIF.
    ENDIF.

  ENDMETHOD.
ENDCLASS.
