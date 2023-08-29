echo "copy font..."
# Set source and target directories
fonts_dir=$( cd "$( dirname "$0" )" && pwd )
find_command="find \"$fonts_dir\" \( -name '*.[o,t]tf' -or -name '*.pcf.gz' \) -type f -print0"
mkdir -p $HOME/.fonts/
# Copy all fonts to user fonts directory
echo "Copying fonts..."
eval $find_command | xargs -0 -I % cp "%" "$HOME/.fonts/"
# Reset font cache on Linux
if command -v fc-cache @>/dev/null ; then
    echo "Resetting font cache, this may take a moment..."
    fc-cache -f $font_dir
fi
echo "font install done"
