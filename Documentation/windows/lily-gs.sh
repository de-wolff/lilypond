#!@SHELL@
# /etc/profile.d/lily-gs.sh -- Check for GhostScript and GSView


gs550="/usr/windows/gstools/gs5.50"
gs650="/usr/windows/gs/gs6.50/bin"

gsview26="/usr/windows/gstools/gsview"
gsview36="/usr/windows/Ghostgum/GSview"

# Maybe read registry, but that may be hairy?
# 
# $ regtool get \\HKLM\\Software\\CLASSES\\psfile\\shell\\open\\command\\
# "C:\GSTOOLS\GSVIEW\gsview32.exe" "%1"



## we set GS_LIB although the registry keys have been set.
##  

if [ -e "$gs550/gswin32.exe" ]; then
	PATH="$gs550:$PATH"
    GS_LIB='C:\cygwin\usr\windows\gs\gs5.50\lib'
fi

if [ -e "$gs650/gswin32.exe" ]; then
	PATH="$gs650:$PATH"
    GS_LIB='C:\cygwin\usr\windows\gs\gs6.50\lib'
fi

if [ -e "$gsview26/gsview32.exe" ]; then
	PATH="$gsview26:$PATH"
fi

if [ -e "$gsview36/gsview32.exe" ]; then
	PATH="$gsview36:$PATH"
fi




export GS_LIB 
export PATH 
