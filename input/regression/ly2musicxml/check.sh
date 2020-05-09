#!/bin/bash

# $1 is xml file under test
# $2 is source path
# $3 is location of our catalog file

information="''"
base=${1%.*}
xpathfile=$base.xpath
regexfile=$base.regex
infofile=$base.info
logfile=$(basename ${1%.*}).log
content=$(<$1)

export XML_CATALOG_FILES="$3/catalog.xml  /etc/xml/catalog"

if [ -e "$infofile" ] ;
then
    tmp=$(<$infofile)
    echo $tmp >> $logfile
    information=$(echo while $tmp)
else
    information="''"
fi

#if a regex file is present, validate using this regex
if [ -e $regexfile ] ;
then
    regex=$(<$regexfile)
    if ! [[ $content =~ $regex ]];
    then
        echo $information  >> $logfile
        echo content >> $logfile
        echo $content >> $logfile
        echo does not match >> $logfile
        echo $regex >> $logfile
        exit 1
    else
        echo xml is checked against expected regular expression: >> $logfile
        echo "$regex" >> $logfile
        echo >> $logfile
    fi
fi

#if a xpath file is present, validate using this regex
if [ -e $xpathfile ] ;
then
    xpath=$(<$xpathfile)
    xsltproc --stringparam xpath "$xpath" --stringparam \
    information "$information" $2/xpath.xslt $1 >> $logfile
    if (( $? != 0 ));
    then
        echo not matching expected xpath >> $logfile
        exit 2
    else
        echo xml is checked against expected xpath: >> $logfile
        echo "$xpath" >> $logfile
        echo >> $logfile
    fi
fi
if ! ( [ -e $xpathfile ] || [ -e $regexfile ] )
then
    echo no checks are found for $1 >> $logfile
    echo no checks are found for $1
    exit 3
fi

# if xml has a DTD reference, validate xml using dtd
regex='\<\!DOCTYPE[^\>]*PUBLIC[[:space:]]*\"([^\"]*)\"[[:space:]]*\"([^\"]*)\"[[:space:]]*\>'
if [[ $content =~ $regex ]];
then
    if  ! $(xmllint --valid --noout $1 >>$logfile);
    then
        echo DTD error for $1 : >>$logfile
        echo ${BASH_REMATCH[0]} >>$logfile
        exit 4
    else
        echo xml syntax is validated against the dtd at ${BASH_REMATCH[2]} >>$logfile
    fi
fi

# if xml has a XSD reference, validate xml using xsd
regex='\<[^\>]*[[:space:]][[:alpha:]]*:schemaLocation=\"([^ ]*)[[:space:]]([^ \"]*)'
if [[ $content =~ $regex ]];
then
    if ! $(xmllint --schema ${BASH_REMATCH[1]}/${BASH_REMATCH[2]} --noout $1 >>$logfile);
    then
        echo xsd error for $1 : >>$logfile
        echo ${BASH_REMATCH[0]} >>$logfile
        exit 5
    else
        echo xml syntax is validated against the xsd at ${BASH_REMATCH[1]}/${BASH_REMATCH[2]} >>$logfile
    fi
fi