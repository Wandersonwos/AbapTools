CLASS zcl_abap2xlsx_excel_tools DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    CLASS-METHODS:
      gen_excel_link_style
        IMPORTING
          io_excel                  TYPE REF TO zcl_excel
        RETURNING
          VALUE(rv_style_link_guid) TYPE zexcel_cell_style.

  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_abap2xlsx_excel_tools IMPLEMENTATION.
  METHOD gen_excel_link_style.

    DATA(lv_style_link)                 = io_excel->add_new_style( ).
    lv_style_link->font->underline      = abap_true.
    lv_style_link->font->underline_mode = zcl_excel_style_font=>c_underline_single.
    lv_style_link->font->color-rgb      = zcl_excel_style_color=>c_blue.
    rv_style_link_guid                  = lv_style_link->get_guid( ).

  ENDMETHOD.

ENDCLASS.
