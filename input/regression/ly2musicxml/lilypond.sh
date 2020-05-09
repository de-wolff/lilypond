#!/bin/bash
lilypond="$1"
shift
export LILYPOND_DEBUG=true
export XML_LIBRARY_DEBUG=true
$lilypond --loglevel=ERROR $@