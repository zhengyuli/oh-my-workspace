#!/bin/bash

#---------------------------------------------------------------------------------
# Name: install.sh
# Purpose: Emacs configurations setup script
#
# Time-stamp: <2023-05-26 11:41:11 Friday by zhengyuli>
#
# Author: zhengyu li
# Created: 2014-03-26
#
# Copyright (c) 2014, 2022, 2023 zhengyu li <lizhengyu419@gmail.com>
#---------------------------------------------------------------------------------

source /etc/profile
export LC_ALL=C

set -e

BASE_DIR=$(cd $(dirname $0); pwd)
EMACS_CONFIG_FILE=$HOME/.emacs
EMACS_HTTP_PROXY=$(printenv http_proxy)

SED_CMD="sed -i"

if [[ "$OSTYPE" == "darwin"* ]]; then
    SED_CMD="sed -i ''"
fi

cp -v ${BASE_DIR}/init.el ${EMACS_CONFIG_FILE}
${SED_CMD} s#init\\.el#\\.emacs#g ${EMACS_CONFIG_FILE}
${SED_CMD} s#_EMACS_CONFIG_ROOT_PATH_#${BASE_DIR}/#g ${EMACS_CONFIG_FILE}
${SED_CMD} s#_EMACS_HTTP_PROXY_#${EMACS_HTTP_PROXY}#g ${EMACS_CONFIG_FILE}

echo "Install successfully!!"
