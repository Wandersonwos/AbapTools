CLASS zcl_ddic_tools DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    CLASS-METHODS:
      get_domain_texts
        IMPORTING
          domname                   TYPE domname
          text                      TYPE ddbool_d DEFAULT abap_true
          langu                     TYPE ddlanguage DEFAULT sy-langu
        RETURNING
          VALUE(fixed_values_texts) TYPE dd07v_tab.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_ddic_tools IMPLEMENTATION.
  METHOD get_domain_texts.

    CALL FUNCTION 'DD_DOMVALUES_GET'
      EXPORTING
        domname        = domname
        text           = text
        langu          = langu
      TABLES
        dd07v_tab      = fixed_values_texts
      EXCEPTIONS
        wrong_textflag = 1
        OTHERS         = 2.

  ENDMETHOD.

ENDCLASS.
