CLASS z_sy_feature DEFINITION
  PUBLIC
  ABSTRACT
  CREATE PUBLIC

  GLOBAL FRIENDS z_sy_cl_feature_loader .

  PUBLIC SECTION.

    CLASS-METHODS class_constructor .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS z_sy_feature IMPLEMENTATION.

  METHOD class_constructor.
    "new z_sy_cl_feature_loader( new z_sy_feature( ) )->loadFeatureToggles( ).
  ENDMETHOD.

ENDCLASS.
