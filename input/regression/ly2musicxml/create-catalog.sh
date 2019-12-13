#!/bin/bash

cat <<END > $1
<?xml version="1.0" encoding="UTF-8"?>
<!DOCTYPE catalog PUBLIC "-//OASIS//DTD XML Catalogs V1.1//EN" "http://www.oasis-open.org/committees/entity/release/1.1/catalog.dtd">
<catalog xmlns="urn:oasis:names:tc:entity:xmlns:xml:catalog">
      <rewriteSystem
        systemIdStartString="http://www.musicxml.org/dtds"
        rewritePrefix="./musicxml/schema"
      />
      <rewriteURI
        uriStartString="http://www.musicxml.org/xsd"
        rewritePrefix="./musicxml/schema"
      />
      <rewriteSystem
        systemIdStartString="http://www.root.org/dtds"
        rewritePrefix="$2/schema"
      />
</catalog>
END