@echo off
cls

if NOT EXIST .paket\paket.exe (
	echo "not exist"
	.paket\paket.bootstrapper.exe prerelease
	if errorlevel 1 (
	  exit /b %errorlevel%
	)
	)

.paket\paket.exe install
if errorlevel 1 (
   exit /b %errorlevel%
)

packages\FAKE\tools\FAKE.exe build.fsx %*
