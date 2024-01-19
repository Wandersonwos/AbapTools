CLASS zcl_sm30_tools DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    CLASS-METHODS show_docu
      IMPORTING
        iv_table          TYPE c
        iv_aldoc          TYPE c OPTIONAL
        iv_dock_extension TYPE i  DEFAULT 400
        iv_spras          TYPE sylangu  DEFAULT sy-langu.
  PROTECTED SECTION.
  PRIVATE SECTION.

    CLASS-DATA gr_dock TYPE REF TO cl_gui_docking_container .
    CLASS-DATA gr_html TYPE REF TO cl_gui_html_viewer .
    CLASS-DATA gv_table TYPE c .

ENDCLASS.



CLASS zcl_sm30_tools IMPLEMENTATION.
  METHOD show_docu.
* Local data
    DATA lt_lines            TYPE STANDARD TABLE OF tline.
    DATA ls_header           TYPE thead.
    DATA lt_html             TYPE STANDARD TABLE OF  htmlline.
    DATA lv_url              TYPE c LENGTH 500.
    DATA lv_table            TYPE doku_obj.
    DATA lv_spras            TYPE sylangu.
    DATA lt_conv_charformats TYPE TABLE OF tline.
    DATA lt_conv_parformats  TYPE TABLE OF tline.


* has anything changed??
    CHECK gv_table <> iv_table.

    lv_table = iv_table.

*  Read table docu
    CALL FUNCTION 'DOCU_GET'
      EXPORTING
        id     = 'TB'
        langu  = iv_spras
        object = lv_table
      IMPORTING
        head   = ls_header
      TABLES
        line   = lt_lines
      EXCEPTIONS
        OTHERS = 5.
    IF sy-subrc > 0.
* try other language
      CASE iv_spras.
        WHEN 'P'.
          lv_spras = 'E'.
        WHEN 'D'.
          lv_spras = 'E'.
        WHEN 'E'.
          lv_spras = 'P'.
        WHEN 'F'.
          lv_spras = 'E'.
      ENDCASE.
* read docu in other language
      CALL FUNCTION 'DOCU_GET'
        EXPORTING
          id     = 'TB'
          langu  = lv_spras
          object = lv_table
        IMPORTING
          head   = ls_header
        TABLES
          line   = lt_lines
        EXCEPTIONS
          OTHERS = 5.
    ENDIF.

*  Read alternative docu
    IF lt_lines IS INITIAL AND iv_aldoc IS NOT INITIAL.
* read alternative docu (Dialog Text)
      lv_table = iv_aldoc.
      CALL FUNCTION 'DOCU_GET'
        EXPORTING
          id     = 'DT'
          langu  = sy-langu
          object = lv_table
        IMPORTING
          head   = ls_header
        TABLES
          line   = lt_lines
        EXCEPTIONS
          OTHERS = 5.
      IF sy-subrc > 0.
* read alternative docu (Dialog Text) in different language
        CALL FUNCTION 'DOCU_GET'
          EXPORTING
            id     = 'DT'
            langu  = lv_spras
            object = lv_table
          IMPORTING
            head   = ls_header
          TABLES
            line   = lt_lines
          EXCEPTIONS
            OTHERS = 5.

      ENDIF.
    ENDIF.

* _____________________ *
*                                                                 *
*  Build controls
* _____________________ *
*                                                                 *

    IF gr_dock IS INITIAL.
* create docking container
      CREATE OBJECT gr_dock
        EXPORTING
          side                    = cl_gui_docking_container=>dock_at_right
          extension               = iv_dock_extension
          no_autodef_progid_dynnr = 'X'.
    ENDIF.


    IF lt_lines IS INITIAL.
* No doku: Set controls to invisible
      IF gr_html IS BOUND.
        CALL METHOD gr_html->set_visible
          EXPORTING
            visible = space.
      ENDIF.

      IF gr_dock IS BOUND.
        CALL METHOD gr_dock->set_visible
          EXPORTING
            visible = space.
      ENDIF.
    ELSE.
* Doku exists: Set controls visible
      IF gr_html IS BOUND.
        CALL METHOD gr_html->set_visible
          EXPORTING
            visible = 'X'.
      ENDIF.
      IF gr_dock IS BOUND.
        CALL METHOD gr_dock->set_visible
          EXPORTING
            visible = 'X'.
      ENDIF.
    ENDIF.


    IF lt_lines IS NOT INITIAL.
* doku exists:
      IF gr_html IS INITIAL.
* Create HTML-Control
        CREATE OBJECT gr_html
          EXPORTING
            parent = gr_dock.
      ENDIF.

* _____________________ *
*                                                                 *
*  Convert character and parameter formats
* _____________________ *
*                                                                 *
      IF lt_conv_parformats IS INITIAL.
        PERFORM build_mapping_tables   IN PROGRAM rshtmimg_2
         TABLES lt_conv_charformats
                lt_conv_parformats.
      ENDIF.

* _____________________ *
*                                                                 *
*  Convert Docu to HTML
* _____________________ *
*                                                                 *

      CALL FUNCTION 'CONVERT_ITF_TO_HTML'
        EXPORTING
          i_header           = ls_header
        TABLES
          t_itf_text         = lt_lines
          t_html_text        = lt_html
          t_conv_charformats = lt_conv_charformats
          t_conv_parformats  = lt_conv_parformats
        EXCEPTIONS
          syntax_check       = 1
          replace            = 2
          illegal_header     = 3
          OTHERS             = 4.
      IF sy-subrc = 0.

* Convert Tables
        PERFORM convert_tables IN PROGRAM rshtmimg_2 TABLES lt_html.
* Set colours (Make text look like SAP documentation)
        PERFORM set_colors     IN PROGRAM rshtmimg_2 TABLES lt_html.

* Push data to control
        CALL METHOD gr_html->load_data
          IMPORTING
            assigned_url = lv_url
          CHANGING
            data_table   = lt_html
          EXCEPTIONS
            OTHERS       = 4.

        IF sy-subrc = 0.
* _____________________ *
*                                                                 *
*  Display HTML-Text
* _____________________ *
*                                                                 *

          CALL METHOD gr_html->show_url
            EXPORTING
              url = lv_url.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDMETHOD.

ENDCLASS.
