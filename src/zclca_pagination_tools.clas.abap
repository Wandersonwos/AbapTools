CLASS zclca_pagination_tools DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    CLASS-METHODS:
      get_next_block_lines
        IMPORTING
          !i_block_size         TYPE i
          !i_index              TYPE i
        EXPORTING
          !e_initial_block_line TYPE i
          !e_final_block_line   TYPE i .

    METHODS:
     constructor
      IMPORTING
        i_block_size TYPE i
        i_index TYPE i OPTIONAL,
      get_next_block
        IMPORTING
          i_total_data TYPE INDEX TABLE
        EXPORTING
          e_block_data TYPE INDEX TABLE.

  PRIVATE SECTION.

    DATA:
      gv_block_size TYPE i,
      gv_index      TYPE i.

ENDCLASS.



CLASS ZCLCA_PAGINATION_TOOLS IMPLEMENTATION.


  METHOD constructor.

    me->gv_block_size = i_block_size.
    me->gv_index = i_index.

  ENDMETHOD.


  METHOD get_next_block.

    DATA:
      lv_initial_block_line TYPE i,
      lv_final_block_line   TYPE i.

    CLEAR e_block_data.

    me->gv_index = me->gv_index + 1.

    get_next_block_lines(
          EXPORTING
            i_block_size         = me->gv_block_size
            i_index              = me->gv_index
          IMPORTING
            e_initial_block_line = lv_initial_block_line
            e_final_block_line   = lv_final_block_line   ).

    FIELD-SYMBOLS <lw_data> TYPE any.
    LOOP AT i_total_data ASSIGNING <lw_data>
                         FROM lv_initial_block_line
                           TO lv_final_block_line.

      INSERT <lw_data> INTO TABLE e_block_data.

    ENDLOOP.

  ENDMETHOD.


  METHOD get_next_block_lines.

    IF i_index = 1.
      e_initial_block_line = 1.
    ELSE.
      e_initial_block_line = i_block_size * ( i_index - 1 ) + 1.
    ENDIF.

    e_final_block_line = e_initial_block_line + i_block_size - 1.

  ENDMETHOD.
ENDCLASS.
