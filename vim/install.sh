#!/bin/bash

#---------------------------------------------------------------------------------
# Name: install.sh
# Purpose: VIM configurations setup script
#
# Time-stamp: <2022-03-17 11:52:33 Thursday by zhengyuli>
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
VIM_CONFIG_FILE=$HOME/.vimrc

cp -v ${BASE_DIR}/vimrc ${VIM_CONFIG_FILE}

echo "Install successfully!!"
