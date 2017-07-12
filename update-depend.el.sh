#!/bin/bash

mv depend.el/ depend.el.bak/                       &&
    git clone https://github.com/jjpe/depend.el    &&
    cd depend.el/                                  &&
    rm -rf .git                                    &&
    cd ../                                         &&
    rm -rf depend.el.bak/                          &&
    echo "[UPDATE-DEPEND.EL] done"
