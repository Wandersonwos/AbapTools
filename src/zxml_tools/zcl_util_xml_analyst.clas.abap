CLASS zcl_util_xml_analyst DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    TYPES:
      BEGIN OF ty_xml_values,
          node_name TYPE string,
          value     TYPE string,
          path      TYPE string,
          line_no   TYPE i,
          length    TYPE i,
      END OF ty_xml_values,
      tty_xml_values        TYPE STANDARD TABLE OF ty_xml_values WITH DEFAULT KEY.

    METHODS:
     constructor
        IMPORTING
            i_xml_string    TYPE string    OPTIONAL
            i_xml_xstring   TYPE xstring   OPTIONAL
            i_xml_path_file TYPE localfile OPTIONAL
            i_path_line_id  TYPE string DEFAULT 'Line/ID' ##no_text
            i_exclude_lines  TYPE trext_parameter_value OPTIONAL
            RAISING
            cx_t100_msg,
     get_root_node_name
            RETURNING
            value(r_root_node_name) TYPE string,
        get_value_with_bars
          IMPORTING
            i_value TYPE string
          RETURNING
            value(r_value) TYPE string,
     get_values_as_table
      IMPORTING
        i_path_without_root TYPE abap_bool DEFAULT abap_true
      RETURNING
        value(r_xml_values) TYPE tty_xml_values,
    get_list_value_by_path
      IMPORTING
        i_node_path TYPE drf_path_name
        i_path_without_root TYPE abap_bool DEFAULT abap_true
        i_skip_lines TYPE i OPTIONAL
        i_skip_lines_c TYPE clike OPTIONAL
        RETURNING
        value(r_value_tab) TYPE string_table,
    get_single_value_by_path
      IMPORTING
        i_node_path         TYPE drf_path_name
        i_path_without_root TYPE abap_bool DEFAULT abap_true
        i_prefix_to_remove  TYPE string OPTIONAL
      RETURNING
        value(r_value) TYPE string,
    get_xml_document
      RETURNING
        value(r_xml_document) TYPE REF TO cl_xml_document.

  PRIVATE SECTION.

    METHODS get_xml_value_iterator
      IMPORTING
        i_node_obj TYPE REF TO if_ixml_node
      RETURNING
        value(r_iterator_obj) TYPE REF TO if_ixml_node_iterator.
    METHODS remove_root_node_name
      CHANGING
        c_xml_value_path TYPE zcl_util_xml_analyst=>ty_xml_values-path.
    METHODS remove_lines
    CHANGING ct_xml_values TYPE tty_xml_values.

    DATA:
      go_xml          TYPE REF TO cl_xml_document,
      gt_xml_values   TYPE tty_xml_values,
      gv_path_line_id TYPE string,
      gt_filter_lines TYPE trext_parameter_value.

ENDCLASS.



CLASS ZCL_UTIL_XML_ANALYST IMPLEMENTATION.


  METHOD constructor.

    DATA lv_retcode TYPE sysubrc.

    CREATE OBJECT go_xml.

    IF i_xml_xstring IS NOT INITIAL.

      lv_retcode = go_xml->parse_xstring( i_xml_xstring ).

    ELSEIF i_xml_string IS NOT INITIAL.

      lv_retcode = go_xml->parse_string( i_xml_string ).

    ELSEIF i_xml_path_file IS NOT INITIAL.

      lv_retcode = go_xml->import_from_file( i_xml_path_file ).

    ENDIF.

    IF lv_retcode <> 0.

      MESSAGE e288(ba) INTO sy-tvar9.
      " Erro durante a conversão dos dados de controle do file de arquivo &1

      RAISE EXCEPTION TYPE cx_t100_msg
        EXPORTING
          t100_msgid = sy-msgid
          t100_msgno = sy-msgno.

    ENDIF.

    me->gv_path_line_id = i_path_line_id.
    me->gt_filter_lines = i_exclude_lines.

  ENDMETHOD.


  METHOD get_list_value_by_path.

    DATA:
      lv_counter    TYPE i,
      lv_skip_lines LIKE i_skip_lines.

    IF me->gt_xml_values IS INITIAL.
      me->gt_xml_values = me->get_values_as_table( ).
    ENDIF.

    lv_skip_lines = i_skip_lines.

    IF i_skip_lines_c IS NOT INITIAL.
      TRY.
          lv_skip_lines = i_skip_lines_c.
        CATCH cx_root.
      ENDTRY.
    ENDIF.

    FIELD-SYMBOLS <lw_xml_value> LIKE LINE OF gt_xml_values.
    LOOP AT gt_xml_values ASSIGNING <lw_xml_value>
                          WHERE path = i_node_path.

      lv_counter = lv_counter + 1.

      IF lv_skip_lines IS NOT INITIAL AND lv_counter > lv_skip_lines.
        EXIT.
      ENDIF.

      APPEND <lw_xml_value>-value TO r_value_tab.

    ENDLOOP.

  ENDMETHOD.


  METHOD get_root_node_name.

    r_root_node_name = go_xml->m_document->get_root( )->get_first_child( )->get_name( ).

  ENDMETHOD.


  METHOD get_single_value_by_path.

    DATA: le_values TYPE ty_xml_values,
          dummy1.

    FIELD-SYMBOLS <lw_xml_value> LIKE LINE OF gt_xml_values.

    IF me->gt_xml_values IS INITIAL.
      me->gt_xml_values = me->get_values_as_table( i_path_without_root ).
    ENDIF.

    IF i_node_path(5) = 'Note#' .

      LOOP AT gt_xml_values ASSIGNING <lw_xml_value>
                            WHERE path = i_node_path(4).

        IF <lw_xml_value>-value CS i_node_path+4(26) AND ( <lw_xml_value>-value CS '#TENSAO#' OR <lw_xml_value>-value CS '#TARIFARIO#' ). "Tensão
          SPLIT <lw_xml_value>-value AT '#' INTO dummy1
                                                 dummy1
                                                 dummy1
                                                 <lw_xml_value>-value
                                                 dummy1.
          r_value = <lw_xml_value>-value.
          EXIT.
        ELSEIF <lw_xml_value>-value CS i_node_path+4(23) AND <lw_xml_value>-value CS '#CPE#'. "CPE
          SPLIT <lw_xml_value>-value AT '#' INTO dummy1
                                                 dummy1
                                                 dummy1
                                                 r_value
                                                 dummy1.
          CONDENSE r_value NO-GAPS.
          EXIT.
        ELSE.
*          CLEAR: <lw_xml_value>.
          r_value = <lw_xml_value>-value.
        ENDIF.
      ENDLOOP.

*      READ TABLE gt_xml_values ASSIGNING <lw_xml_value>
*                             WITH KEY path = i_node_path(4).
*                                      value(26) = i_node_path+4(26).
      CHECK NOT <lw_xml_value> IS INITIAL.
    ELSE.

      READ TABLE gt_xml_values ASSIGNING <lw_xml_value>
                               WITH KEY path = i_node_path.
      IF sy-subrc IS INITIAL.
        r_value = <lw_xml_value>-value.
      ENDIF.

      CHECK sy-subrc = 0.
    ENDIF.

*    r_value = <lw_xml_value>-value.

    IF i_prefix_to_remove IS NOT INITIAL.

      SHIFT r_value LEFT DELETING LEADING i_prefix_to_remove.

    ENDIF.

  ENDMETHOD.


  METHOD get_values_as_table.

    DATA:
        lo_node           TYPE REF TO if_ixml_node,
        lo_iterator       TYPE REF TO if_ixml_node_iterator,
        lv_value          TYPE string,
        lv_char           TYPE char2,
        lv_line_number    TYPE i,
        lw_xml_values LIKE LINE OF r_xml_values.

    lo_node = go_xml->m_document.

    CHECK lo_node IS BOUND.

    lo_iterator = me->get_xml_value_iterator( lo_node ).

    lo_node = lo_iterator->get_next( ).

    WHILE lo_node IS BOUND.

      lv_value = lo_node->get_value( ).

      MOVE lv_value TO lv_char.

      IF lv_char <> cl_abap_char_utilities=>cr_lf AND
         lv_char <> cl_abap_char_utilities=>newline.

        lw_xml_values-node_name = lo_node->get_parent( )->get_name( ).
        lw_xml_values-value     = lv_value.
        lw_xml_values-path      = go_xml->get_node_path( lo_node ).
        lw_xml_values-length    = strlen( lw_xml_values-path ).

        IF i_path_without_root = abap_true.
          me->remove_root_node_name( CHANGING c_xml_value_path = lw_xml_values-path ).
        ENDIF.

        IF lw_xml_values-path = gv_path_line_id.
          lv_line_number = lv_line_number + 1.
        ENDIF.

        lw_xml_values-line_no = lv_line_number.

        APPEND lw_xml_values TO r_xml_values.
        CLEAR  lw_xml_values.

      ENDIF.

      lo_node = lo_iterator->get_next( ).

    ENDWHILE.

    me->remove_lines( CHANGING ct_xml_values = r_xml_values ).

  ENDMETHOD.


  METHOD get_value_with_bars.

    CONCATENATE '/' i_value '/' INTO r_value.

  ENDMETHOD.


  METHOD get_xml_document.

    r_xml_document = me->go_xml.

  ENDMETHOD.


  METHOD get_xml_value_iterator.

    r_iterator_obj = i_node_obj->create_iterator( ).

    r_iterator_obj->set_filter( i_node_obj->create_filter_or(
        filter1 = i_node_obj->create_filter_node_type( if_ixml_node=>co_node_text )
        filter2 = i_node_obj->create_filter_node_type( if_ixml_node=>co_node_cdata_section ) ) ).

  ENDMETHOD.


  METHOD remove_lines.

    DATA lw_xml_line LIKE LINE OF me->gt_xml_values.

    FIELD-SYMBOLS <lw_filter> LIKE LINE OF me->gt_filter_lines.

    LOOP AT me->gt_filter_lines ASSIGNING <lw_filter>.

      LOOP AT ct_xml_values INTO lw_xml_line
                            WHERE node_name = <lw_filter>-parameter
                              AND value     = <lw_filter>-value.

        DELETE ct_xml_values WHERE line_no = lw_xml_line-line_no.

      ENDLOOP.
    ENDLOOP.

  ENDMETHOD.


  METHOD remove_root_node_name.

    DATA: lv_length_root_name TYPE i,
          lv_length_root_name_with_bars TYPE i,
          lv_root_name TYPE string,
          lv_root_node_name_with_bars TYPE string.

    CHECK c_xml_value_path IS NOT INITIAL.

    lv_root_name                  = me->get_root_node_name( ).
    lv_root_node_name_with_bars   = me->get_value_with_bars( lv_root_name ).
    lv_length_root_name_with_bars = strlen( lv_root_node_name_with_bars ).

    IF strlen( c_xml_value_path ) > lv_length_root_name_with_bars.
      SHIFT c_xml_value_path LEFT BY lv_length_root_name_with_bars PLACES.
    ENDIF.

    lv_length_root_name = strlen( lv_root_name ).

    IF strlen( c_xml_value_path ) > lv_length_root_name AND c_xml_value_path(lv_length_root_name) = lv_root_name.
      SHIFT c_xml_value_path LEFT BY lv_length_root_name PLACES.
    ENDIF.

  ENDMETHOD.
ENDCLASS.
