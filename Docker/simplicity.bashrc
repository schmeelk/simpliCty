#!/bin/bash

simplicty() {

        param=$(echo $1 | awk '{$1=$1};1') # Use awk to eat extra end of line characters
        echo -e "\n"                       # Print newline
        if [ -z "$param" ]                 # If no name was provided, use default
        then
          echo -e "No name provided. A Docker generated container name will be used!\n"
          echo -e "Launching new simpliCty build environment.\n"
          echo -e "------------------------------------------\n"
          docker run -t -i simplicty/buildenv:latest /bin/bash

        else
          echo "Launching new simpliCty build environment named: ${param}"
          echo -e "-----------------------------------------------------\n"
          docker run --name $param -t -i simplicty/buildenv:latest /bin/bash
        fi


}

alias simplicty="simplicty"
