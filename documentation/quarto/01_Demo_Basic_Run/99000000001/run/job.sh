#!/bin/bash

# redirect output
exec 3>&1
exec &> "/data/workflows/PEcAn_99000000001/out/99000000001/logfile.txt"

# host specific setup


# cdo setup


# create output folder
mkdir -p "/data/workflows/PEcAn_99000000001/out/99000000001"

# Convert any relative paths to absolute
# (otherwise we'd lose track of them when cd'ing into rundir)
OUTDIR=$(cd "/data/workflows/PEcAn_99000000001/out/99000000001" && pwd -P)
RUNDIR=$(cd "/pecan/documentation/quarto/01_Demo_Basic_Run/99000000001/run" && pwd -P)
SITE_MET=$(cd $(dirname "/data/dbfiles/AmerifluxLBL_SIPNET_site_0-772/AMF_US-NR1_BASE_HH_23-5.2004-01-01.2004-12-31.clim") && pwd -P)/$(basename "/data/dbfiles/AmerifluxLBL_SIPNET_site_0-772/AMF_US-NR1_BASE_HH_23-5.2004-01-01.2004-12-31.clim")
BINARY=$(cd $(dirname "/usr/local/bin/sipnet.git") && pwd -P)/$(basename "/usr/local/bin/sipnet.git")

# see if application needs running
if [ ! -e "${OUTDIR}/sipnet.out" ]; then
  cd "$RUNDIR"
  ln -s "${SITE_MET}" sipnet.clim

  "${BINARY}"
  STATUS=$?
  
  # copy output
  mv "${RUNDIR}/sipnet.out" "$OUTDIR"

  # check the status
  if [ $STATUS -ne 0 ]; then
    echo -e "ERROR IN MODEL RUN\nLogfile is located at '${OUTDIR}/logfile.txt'" >&3
    exit $STATUS
  fi

  # convert to MsTMIP
  echo "require (PEcAn.SIPNET)
    model2netcdf.SIPNET('${OUTDIR}', 40.0329, -105.546, '2004/01/01', '2004/12/31', FALSE, 'git')
    " | R --no-save
fi

# copy readme with specs to output
cp  "${RUNDIR}/README.txt" "${OUTDIR}/README.txt"

# run getdata to extract right variables

# host specific teardown