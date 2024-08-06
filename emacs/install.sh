#!/bin/bash

#---------------------------------------------------------------------------------
# Name: install.sh
# Purpose: Emacs configurations setup script
#
# Time-stamp: <2024-08-06 10:03:16 Tuesday by zhengyu.li>
#
# Author: zhengyu li
# Created: 2014-03-26
#
# Copyright (c) 2014, 2022, 2023, 2024 zhengyu li <lizhengyu419@gmail.com>
#---------------------------------------------------------------------------------

source /etc/profile
export LC_ALL=C

set -e

BASE_DIR=$(cd $(dirname $0); pwd)
EMACS_CONFIG_FILE=$HOME/.emacs

SED_CMD="sed -i"
case "$OSTYPE" in
  darwin*)
    SED_CMD="${SED_CMD} ''"
    ;;
  *)
    ;;
esac

cp -v ${BASE_DIR}/init.el ${EMACS_CONFIG_FILE}
${SED_CMD} s#init\\.el#\\.emacs#g ${EMACS_CONFIG_FILE}
${SED_CMD} s#_EMACS_CONFIG_ROOT_PATH_#${BASE_DIR}/#g ${EMACS_CONFIG_FILE}
if [ x"${http_proxy}" != x ]; then
    ${SED_CMD} "s#emacs-http-proxy nil#emacs-http-proxy \"${http_proxy}\"#g" ${EMACS_CONFIG_FILE}
fi

echo "Install successfully!!"
