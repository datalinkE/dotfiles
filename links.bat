mklink %HOMEDRIVE%%HOMEPATH%\.emacs .emacs
mklink /D %HOMEDRIVE%%HOMEPATH%\.emacs.d %HOMEDRIVE%%HOMEPATH%\AppData\Roaming\.emacs.d
cd .emacs.d
mkdir desktop
