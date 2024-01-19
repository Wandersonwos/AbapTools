CLASS zcl_rtti_tools DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    CLASS-METHODS:
      get_class_name
        IMPORTING
          p_object      TYPE REF TO object
        RETURNING
          VALUE(p_name) TYPE abap_abstypename,
      move_corresp_not_initial
        IMPORTING
          st_from TYPE any
        CHANGING
          st_to   TYPE any,

      create_itab_char
        IMPORTING
          st_rtti_data    TYPE any OPTIONAL
          st_rtti_name    TYPE string OPTIONAL
            PREFERRED PARAMETER st_rtti_data
        EXPORTING
          components      TYPE abap_compdescr_tab
          component_tab   TYPE cl_abap_structdescr=>component_table
          itab_char       TYPE REF TO data
          ddic_field_list TYPE ddfields,
      get_comp_from_struct
        IMPORTING
          st_rtti_data      TYPE any OPTIONAL
          st_rtti_name      TYPE string OPTIONAL
        RETURNING
          VALUE(components) TYPE abap_component_tab.

  PRIVATE SECTION.
    CLASS-METHODS structure_recursive
      IMPORTING
        is_component         TYPE abap_componentdescr
      RETURNING
        VALUE(component_tab) TYPE abap_component_tab.
    CLASS-METHODS structure_case
      IMPORTING
        is_component  TYPE abap_componentdescr
      CHANGING
        component_tab TYPE abap_component_tab .
ENDCLASS.



CLASS zcl_rtti_tools IMPLEMENTATION.


  METHOD create_itab_char.

    DATA:
      lo_desc_estrutura TYPE REF TO cl_abap_structdescr.

*    FIELD-SYMBOLS <lt_itab_char> LIKE itab_char.

    IF st_rtti_name IS NOT INITIAL.
      lo_desc_estrutura = CAST #( cl_abap_typedescr=>describe_by_name( st_rtti_name ) ).
    ELSEIF st_rtti_data IS SUPPLIED.
      lo_desc_estrutura = CAST #( cl_abap_typedescr=>describe_by_data( st_rtti_data ) ).
    ENDIF.

    LOOP AT lo_desc_estrutura->get_components( ) REFERENCE INTO DATA(ls_comp).

      DATA(ls_comp_simple) = REF #( lo_desc_estrutura->components[ name = ls_comp->name ] ).

      APPEND VALUE #( name = ls_comp->name
                      type = SWITCH #( ls_comp_simple->type_kind
                                       WHEN cl_abap_typedescr=>typekind_char
                                         OR cl_abap_typedescr=>typekind_date
                                       THEN ls_comp->type
                                       ELSE cl_abap_elemdescr=>get_c( ls_comp_simple->length + ls_comp_simple->decimals + 5 ) ) ) TO component_tab.
    ENDLOOP.

    DATA(lo_line_type_dyn)  = cl_abap_structdescr=>create( component_tab ).
    DATA(lo_table_type_dyn) = cl_abap_tabledescr=>create( lo_line_type_dyn ).

    CREATE DATA itab_char TYPE HANDLE lo_table_type_dyn.

    components      = lo_desc_estrutura->components.
    ddic_field_list = lo_desc_estrutura->get_ddic_field_list( ).

  ENDMETHOD.


  METHOD get_class_name.
    p_name = cl_abap_classdescr=>get_class_name( p_object ).
    SHIFT p_name LEFT DELETING LEADING cl_uml_class_scanner=>c_rtti_class.
  ENDMETHOD.


  METHOD move_corresp_not_initial.

    TRY.
        DATA(lo_structdescr) = CAST cl_abap_structdescr( cl_abap_structdescr=>describe_by_data( st_from ) ).

        LOOP AT lo_structdescr->components REFERENCE INTO DATA(ls_comp).
          ASSIGN COMPONENT ls_comp->name OF STRUCTURE st_from TO FIELD-SYMBOL(<lv_field_from>).
          CHECK sy-subrc = 0.

          ASSIGN COMPONENT ls_comp->name OF STRUCTURE st_to TO FIELD-SYMBOL(<lv_field_to>).
          CHECK sy-subrc = 0.

          CHECK <lv_field_from> IS NOT INITIAL.

          <lv_field_to> = <lv_field_from>.

        ENDLOOP.

      CATCH cx_root.
    ENDTRY.


  ENDMETHOD.
  METHOD get_comp_from_struct.

    DATA:
      lt_comp_str       TYPE abap_component_tab,
      lo_desc_estrutura TYPE REF TO cl_abap_structdescr.

    IF st_rtti_name IS NOT INITIAL.
      lo_desc_estrutura = CAST #( cl_abap_typedescr=>describe_by_name( st_rtti_name ) ).
    ELSEIF st_rtti_data IS SUPPLIED.
      CASE TYPE OF cl_abap_typedescr=>describe_by_data( st_rtti_data ).
        WHEN TYPE cl_abap_structdescr INTO lo_desc_estrutura.
        WHEN TYPE cl_abap_tabledescr INTO DATA(lo_desc_table).
          lo_desc_estrutura = CAST #( lo_desc_table->get_table_line_type( ) ).
      ENDCASE.
    ENDIF.

    CHECK lo_desc_estrutura IS BOUND.

    LOOP AT lo_desc_estrutura->get_components( ) REFERENCE INTO DATA(ls_comp).

      structure_case( EXPORTING is_component = ls_comp->*
                      CHANGING component_tab = components ).
    ENDLOOP.

  ENDMETHOD.


  METHOD structure_recursive.
    DATA: lo_struct     TYPE REF TO cl_abap_structdescr,
          lt_components TYPE abap_component_tab,
          ls_components TYPE abap_componentdescr.

    REFRESH component_tab.

    lo_struct ?= is_component-type.
    lt_components = lo_struct->get_components( ).

    LOOP AT lt_components INTO ls_components.
      structure_case( EXPORTING is_component  = ls_components
                      CHANGING  component_tab = component_tab ) .
    ENDLOOP.
  ENDMETHOD.

  METHOD structure_case.

    DATA: lt_comp_str        TYPE abap_component_tab.

    CASE is_component-type->kind.
      WHEN cl_abap_typedescr=>kind_elem. "E Elementary Type
        INSERT is_component INTO TABLE component_tab.
      WHEN cl_abap_typedescr=>kind_table. "T Table
        INSERT is_component INTO TABLE component_tab.
      WHEN cl_abap_typedescr=>kind_struct. "S Structure
        lt_comp_str = structure_recursive( is_component = is_component ).
        INSERT LINES OF lt_comp_str INTO TABLE component_tab.
      WHEN OTHERS. "cl_abap_typedescr=>kind_ref or  cl_abap_typedescr=>kind_class or  cl_abap_typedescr=>kind_intf.
* We skip it. for now.
    ENDCASE.

  ENDMETHOD.

ENDCLASS.
