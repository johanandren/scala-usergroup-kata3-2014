set SBT_OPTS=
set SCRIPT_DIR=%~dp0\
java -Xmx512M -jar "%SCRIPT_DIR%sbt-launch.jar" %*
