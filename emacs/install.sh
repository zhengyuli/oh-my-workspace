#!/bin/bash

#---------------------------------------------------------------------------------
# Name: install.sh
# Purpose: Emacs configurations setup script
#
# Time-stamp: <2020-11-24 09:55:32 Tuesday by lizhengyu>
#
# Author: zhengyu li
# Created: 2014-03-26
#
# Copyright (c) 2014 zhengyu li <lizhengyu419@gmail.com>
#---------------------------------------------------------------------------------

source /etc/profile
export LC_ALL=C

set -e

BASE_DIR=$(cd $(dirname $0); pwd)
EMACS_CONFIG_FILE=$HOME/.emacs

cp ${BASE_DIR}/init.el ${EMACS_CONFIG_FILE}

if [[ "$OSTYPE" == "darwin"* ]]; then
	sed -i '' s:_EMACS_CONFIG_ROOT_PATH_:${BASE_DIR}/:g ${EMACS_CONFIG_FILE}
else
	sed -i s:_EMACS_CONFIG_ROOT_PATH_:${BASE_DIR}/:g ${EMACS_CONFIG_FILE}
fi

echo "Success!!"
