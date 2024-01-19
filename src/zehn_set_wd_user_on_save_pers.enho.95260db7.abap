"Name: \TY:CL_WDR_CONFIG_DATA_CONTAINER\IN:IF_WDR_CONFIG_DATA_CONTAINER\ME:SAVE\SE:BEGIN\EI
ENHANCEMENT 0 ZEHN_SET_WD_USER_ON_SAVE_PERS.

DATA(lv_real_wd_user) = zcl_wda_tools=>get_real_wd_user( fixed_user = sy-uname ).

IF lv_real_wd_user IS NOT INITIAL.

  DATA(lv_sy_uname_old) = sy-uname.

  sy-uname = lv_real_wd_user.

  if_wdr_config_data_container~save(
    EXPORTING
      devclass  = devclass
      transport = transport
    IMPORTING
      saved     = saved
      messages  = messages
  ).

  sy-uname = lv_sy_uname_old.

  RETURN.
ENDIF.

ENDENHANCEMENT.
