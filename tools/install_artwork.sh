#!/bin/bash

# This script is for converting to the artwork/icon.xcf file to a PNG and
# resizing it to the various dimensions for res/drawable-$cat/ic_launcher.png.

readonly BASE_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)/.."


check_deps() {
    if [ ! $(which xcf2png) ]; then
        echo "The program 'xcf2png' is currently not installed."
        exit 1
    fi

    if [ ! $(which convert) ]; then
        echo "The program 'convert' is currently not installed."
        exit 1
    fi
}


convert_to_png() {
    local xcf_file=$1
    local png_file=$2

    rm -f $png_file
    xcf2png $xcf_file -o $png_file
}


do_resize() {
    local width=$1
    local height=$2
    local icon_png=$3
    local cat=$4
 
    local out_png="$BASE_DIR/app/res/drawable-$cat/ic_launcher.png"

    convert -resize ${width}x${height} $icon_png $out_png
}


main() {
    local icon_xcf="$BASE_DIR/artwork/icon.xcf"
    local icon_png="/tmp/sd_icon.png"

    check_deps

    convert_to_png $icon_xcf $icon_png

    do_resize 48 48 $icon_png "mdpi"
    do_resize 72 72 $icon_png "hdpi"
    do_resize 96 96 $icon_png "xhdpi"
    do_resize 144 144 $icon_png "xxhdpi"
}


main
