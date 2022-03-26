#!/bin/bash

#---------------------------------------------------------------------------------
# Name: install.sh
# Purpose: Emacs configurations setup script
#
# Time-stamp: <2022-03-26 13:20:08 Saturday by zhengyuli>
#
# Author: zhengyu li
# Created: 2014-03-26
#
# Copyright (c) 2014, 2022 zhengyu li <lizhengyu419@gmail.com>
#---------------------------------------------------------------------------------

source /etc/profile
export LC_ALL=C

set -e

BASE_DIR=$(cd $(dirname $0); pwd)
EMACS_CONFIG_FILE=$HOME/.emacs

SED_CMD="sed -i"

if [[ "$OSTYPE" == "darwin"* ]]; then
    SED_CMD="sed -i ''"
fi

cp -v ${BASE_DIR}/init.el ${EMACS_CONFIG_FILE}
${SED_CMD} s:"init\.el":$".emacs":g ${EMACS_CONFIG_FILE}
${SED_CMD} s:_EMACS_CONFIG_ROOT_PATH_:${BASE_DIR}/:g ${EMACS_CONFIG_FILE}

echo "Install successfully!!"
