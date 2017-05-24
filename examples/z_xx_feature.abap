class Z_XX_FEATURE definition
  public
  inheriting from Z_SY_FEATURE
  final.

public section.

  CLASS-DATA feature_a type ABAP_BOOL.
  DATA feature_a_desc type STRING value `Put feature-description here` .

  class-methods CLASS_CONSTRUCTOR .
 
  protected section.
  private section.
ENDCLASS.


CLASS Z_XX_FEATURE IMPLEMENTATION.
  METHOD class_constructor.
    "boilerplate code  
    z_sy_cl_feature_loader=>load( new z_xx_feature( ) ).
  ENDMETHOD.
ENDCLASS.
