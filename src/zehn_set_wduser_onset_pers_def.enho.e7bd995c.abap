"Name: \TY:CL_WDR_ADAPT_PERSIST_HDL_PERS\ME:SAVE_ON_SCOPE\SE:BEGIN\EI
ENHANCEMENT 0 ZEHN_SET_WDUSER_ONSET_PERS_DEF.
DATA(lv_real_wd_user) = zcl_wda_tools=>get_real_wd_user( fixed_user = sy-uname ).

IF lv_real_wd_user IS NOT INITIAL.

  DATA(lv_sy_uname_old) = sy-uname.

  sy-uname = lv_real_wd_user.

  save_on_scope(
    EXPORTING
      content_handler = content_handler
      pers_service    = pers_service
      save_control    = save_control
    RECEIVING
      config_data     = config_data
  ).

  sy-uname = lv_sy_uname_old.

  RETURN.
ENDIF.


ENDENHANCEMENT.
