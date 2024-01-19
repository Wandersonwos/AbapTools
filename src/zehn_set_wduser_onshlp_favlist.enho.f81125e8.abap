"Name: \PR:SAPLSDSD\FO:PERSONAL_VALUES_BUFFER\SE:BEGIN\EI
ENHANCEMENT 0 ZEHN_SET_WDUSER_ONSHLP_FAVLIST.

DATA(lv_real_wd_user) = zcl_wda_tools=>get_real_wd_user( fixed_user = sy-uname ).

IF lv_real_wd_user IS NOT INITIAL.

  DATA(lv_sy_uname_old) = sy-uname.
  sy-uname = lv_real_wd_user.

  PERFORM personal_values_buffer
       TABLES ddshpvalue_tab
       USING function
             shlp.

  sy-uname = lv_sy_uname_old.
  RETURN.
ENDIF.

ENDENHANCEMENT.
