; This module may be used without any modification in any open source project.
; For commercial use a license can be obtained from jasoon, info@jasoon.nl

; This module is implementing parts of the DOM level 2 core definition,
; as can be found on https://www.w3.org/TR/DOM-Level-2-Core/
; Not all functions are implemented.
;
; Although we follow the rules of DOM2, in the documentation we often refer
; to the DOM3 documentation. This is because in the DOM3 specifications more ; tags are added, so it is easy to jump direct to the right specification.


(define-module (scm xml-library)
  #:use-module (ice-9 r5rs)
  #:use-module (ice-9 optargs)
  #:use-module (ice-9 syncase)
  #:use-module (srfi srfi-1)
  #:use-module (oop goops)
  #:export (

;; following exports are not part of the DOM, but are for convenience
<xml:node>
<xml:attribute>
<xml:entity-reference>
<xml:document>
<xml:document-fragment>
<xml:document-type>
<xml:element>
<xml:text>
<xml:cdata>
<xml:comment>
<xml:processing-instruction>

xml:node?
xml:attribute?
xml:entity-reference?
xml:document?
xml:document-fragment?
xml:document-type?
xml:element?
xml:text?
xml:cdata?
xml:comment?
xml:processing-instruction?


xml:write
xml:formatDocument


;; beware: not all exported methods are implemented,
;; but they are part of the DOM 2 definition
;; although there are also unimplemented DOM 2 meths which are not exported
;; because some of the names are so common
;; we prefix all exported function names with xml:

xml:appendChild
xml:attributes
xml:childNodes
xml:cloneNode
xml:firstChild
xml:hasChildNodes
xml:hasAttribute
xml:hasAttributes
xml:insertBefore
xml:lastChild
xml:localName
xml:nextSibling
xml:nodeName
xml:nodeType
xml:nodeValue
xml:normalize ; not implemented
xml:ownerDocument
xml:parentNode
xml:prefix
xml:previousSibling
xml:removeChild
xml:replaceChild

;; introduced in DOM3, but used already here
;; documented on https://www.w3.org/TR/DOM-Level-3-Core/
xml:getUserData
xml:setUserData

;; conforming DOM2 only used for attributes
xml:value

;; conforming DOM2 only used for character-data
xml:data
xml:appendData ; not implemented
xml:insertData ; not implemented
xml:deleteData ; not implemented
xml:replaceData ; not implemented

;; introduced in DOM3, used here
;; documented on https://www.w3.org/TR/DOM-Level-3-Core/

xml:isElementContentWhitespace

;; conforming DOM2 only used for documents

xml:doctype
xml:documentElement
xml:createAttribute
xml:createAttributeNS ; not implemented
xml:createCDATASection
xml:createComment
xml:createDocumentFragment ; not implemented
xml:createElement
xml:createElementNS ; not implemented
xml:createEntityReference ; not implemented
xml:createProcessingInstruction ; not implemented
xml:createTextNode
xml:getElementById ; not implemented

xml:importNode ; not implemented

;; conforming DOM2 only used for elements

xml:getAttribute
xml:getAttributeNode
xml:removeAttribute ; not implemented
xml:setAttribute
xml:setAttributeNode

;; conforming DOM2 only used for documents and elements

xml:getElementsByTagName
xml:getElementsByTagNameNS ; not implemented

;; conforming DOM2 only used for named node maps
xml:setNamedItem
xml:getNamedItem

;; conforming DOM2

xml:createDocument
xml:createDocumentType
))

(debug-enable 'show-file-name 'yes)
(debug-enable 'backtrace)

(define empty '())
(define empty? (lambda (value) (null? value)))
(define not-empty? (lambda (value) (not (null? value))))

; check whether debug is defined.
; if the environment variable XML_LIBRARY_DEBUG is set return true
; otherwise return false
(define check-debug (lambda()
    (not (not (getenv "XML_LIBRARY_DEBUG")))
))

; syntax: assert ?expr ?expr ... [report: ?r-exp ?r-exp ...]
;
; assert is a nop operation if at the moment of loading this module
; the (operating system) environment variable DEBUG was not defined
;
; otherwise :
;
; If (and ?expr ?expr ...) evaluates to anything but #f, the result
; is the value of that expression.
; If (and ?expr ?expr ...) evaluates to #f, an error is reported.
; The error message will show the failed expressions, as well
; as the values of selected variables (or expressions, in general).
; The user may explicitly specify the expressions whose
; values are to be printed upon assertion failure -- as ?r-exp that
; follow the basic-value 'report:'
; Typically, ?r-exp is either a variable or a string constant.
; If the user specified no ?r-exp, the values of variables that are
; referenced in ?expr will be printed upon the assertion failure.
;
(define-syntax assert
  (if (check-debug)
    (syntax-rules (report:)
    ((assert "doit" (expr ...) (r-exp ...))
     (cond
      ((and expr ...) => (lambda (x) x))
      (else
       (throw 'assertion-failure (list '(and expr ...) r-exp ...)))))
    ((assert "collect" (expr ...))
     (assert "doit" (expr ...) ()))
    ((assert "collect" (expr ...) report: r-exp ...)
     (assert "doit" (expr ...) (r-exp ...)))
    ((assert "collect" (expr ...) expr1 stuff ...)
     (assert "collect" (expr ... expr1) stuff ...))
    ((assert stuff ...)
     (assert "collect" () stuff ...)))
    (syntax-rules ()
      ((expr ...)
       '()))))

;; forward declarations

(define-class <xml:node> ())
(define-class <xml:character-data> (<xml:node>))
(define-class <xml:named-node> (<xml:node>))

              ;also implements named-node interface
(define-class <xml:attribute> (<xml:node>))
(define-class <xml:entity-reference> (<xml:node>))
(define-class <xml:document-fragment> (<xml:node>))
(define-class <xml:document-type> (<xml:node>))
(define-class <xml:element> (<xml:node>))
(define-class <xml:text> (<xml:character-data>))
(define-class <xml:cdata> (<xml:character-data>))
(define-class <xml:comment> (<xml:character-data>))
(define-class <xml:processing-instruction> (<xml:character-data>))
(define-class <xml:document> (<xml:node>))
(define-class <xml:entity> (<xml:named-node>))
(define-class <xml:notation> (<xml:named-node>))

;; test predicates

(define xml:attribute? (lambda (self)  (is-a? self <xml:attribute>)))
(define xml:cdata? (lambda (self)  (is-a? self <xml:cdata>)))
(define xml:comment? (lambda (self)  (is-a? self <xml:comment>)))
(define xml:document? (lambda (self)  (is-a? self <xml:document>)))
(define xml:document-fragment? (lambda (self)
    (is-a? self <xml:document-fragment>)))
(define xml:document-type? (lambda (self)
    (is-a? self <xml:document-type>)))
(define xml:element? (lambda (self)  (is-a? self <xml:element>)))
(define xml:entity-reference? (lambda (self)
    (is-a? self <xml:entity-reference>)))
(define xml:processing-instruction? (lambda (self)
    (is-a? self <xml:processing-instruction>)))
(define xml:node? (lambda (self)  (is-a? self <xml:node>)))
(define xml:text? (lambda (self)  (is-a? self <xml:text>)))

;; non public test predicates

(define xml:character-data? (lambda (self)
    (is-a? self <xml:character-data>)))
(define xml:named-node? (lambda (self)  (is-a? self <xml:named-node>)))
(define xml:named-node-map? (lambda (self)
    (is-a? self <xml:named-node-map>)))
(define xml:white-space? (lambda (self)  (is-a? self <xml:white-space>)))

;  in this document the type basic-value is used.
;  In fact basic-value is not a type, but I use it for convenience.
;  A basic value is a type that can be converted to a string, and will be
;  converted to a string when we use (display) on a value of such a type. It
;  can be used as tag or as a value for a attribute.
;  Within the lifetime of the document the value type will not be changed.
;  So when we do a xml:setAttribute with a integer, the xml:getAttribute will
;  return an integer.
;  This can be useful when we want to use xml-nodes to exchange information.
;  At this moment string, symbol and integer can be used as basic-value, but it is easy to extend this in the future.

(define basic-value? (lambda(value)
    (or (string? value)(symbol? value)(integer? value))))


;; todo: test if new children have the same owner document
;; todo: implement allowed children
;
; Attr :
; Text, EntityReference
;
; Document :
; Element (max 1), ProcessingInstruction, Comment, DocumentType (max 1)
;
; DocumentFragment :
; Entity :
; EntityReference :
; Element :
; Element, ProcessingInstruction, Comment, Text, CDATASection, EntityReference
;
; DocumentType :
; ProcessingInstruction :
; Comment :
; Text :
; CDATASection :
; Notation :
;       no children


; For a description see:
; https://www.w3.org/TR/DOM-Level-2-Core/core.html#ID-17189187

(define ExceptionCodes `(
; If index or size is negative, or greater than the allowed value
(INDEX_SIZE_ERR . 1)
; If the specified range of text does not fit into a DOMString
(DOMSTRING_SIZE_ERR . 2)
; If any node is inserted somewhere it doesn't belong
(HIERARCHY_REQUEST_ERR . 3)
; If a node is used in a different document than the one that created it
(WRONG_DOCUMENT_ERR . 4)
; If an invalid or illegal character is specified, such as in a name. See
; https://www.w3.org/TR/1998/REC-xml-19980210#NT-Char in the XML specification
; for the definition of a legal character, and
; https://www.w3.org/TR/1998/REC-xml-19980210#NT-Name for the definition of a
; legal name character.
(INVALID_CHARACTER_ERR . 5)
; If data is specified for a node which does not support data
(NO_DATA_ALLOWED_ERR . 6)
; If an attempt is made to modify an object where modifications are not allowed
(NO_MODIFICATION_ALLOWED_ERR . 7)
;If an attempt is made to reference a node in a context where it does not exist
(NOT_FOUND_ERR . 8)
; If the implementation does not support the requested type of object or
; operation.
(NOT_SUPPORTED_ERR . 9)
; If an attempt is made to add an attribute that is already in use elsewhere
(INUSE_ATTRIBUTE_ERR . 10)
; If an attempt is made to use an object that is not, or is no longer, usable
(INVALID_STATE_ERR . 11)
; If an invalid or illegal string is specified.
(SYNTAX_ERR . 12)
; If an attempt is made to modify the type of the underlying object.
(INVALID_MODIFICATION_ERR . 13)
; If an attempt is made to create or change an object in a way which is
; incorrect with regard to namespaces.
(NAMESPACE_ERR . 14)
; If a parameter or an operation is not supported by the underlying object.
(INVALID_ACCESS_ERR . 15)
))

; see https://www.w3.org/TR/DOM-Level-3-Core/core.html#ID-17189187
; according to the dom specification an exception handling mechanism is
; implemented, for exceptional situations. The exception code is printed, and
; the script will stop.
(define raise-dom-exception (lambda (exception)
  (assert (symbol? exception)
          report: '@raise-dom-exception )
  (throw 'dom-exception (assoc exception ExceptionCodes) "")))

(define raise-dom-exception-msg (lambda (exception message)
  (assert (symbol? exception)
          (string? message)
          report: '@raise-dom-exception-msg )
  (throw 'dom-exception (exception exception ExceptionCodes) message)))

(define NodeTypes `(
  (ELEMENT_NODE . 1)
  (ATTRIBUTE_NODE . 2)
  (TEXT_NODE . 3)
  (CDATA_SECTION_NODE . 4)
  (ENTITY_REFERENCE_NODE . 5)
  (ENTITY_NODE . 6)
  (PROCESSING_INSTRUCTION_NODE . 7)
  (COMMENT_NODE . 8)
  (DOCUMENT_NODE . 9)
  (DOCUMENT_TYPE_NODE . 10)
  (DOCUMENT_FRAGMENT_NODE . 11)
  (NOTATION_NODE . 12)
))

; see: https://www.w3.org/TR/DOM-Level-3-Core/core.html#ID-184E7107
; Adds the xml:node newChild to the end of the list of children of this node.
; If the newChild is already in the tree, it is first removed.
; If it is a xml:document-fragment object, the entire contents of the document
; fragment are moved into the child list of this node

; Parameters
; self: The node which provides the function
; newChild: of type xml:node
;   The node to add.

; Return Value
;   The node added.
(define xml:appendChild (lambda (self new-child)
  (assert (xml:node? self)
          (xml:node? new-child)
          report: '@xml:appendChild)
  (node:appendChild self new-child)))

; see: https://www.w3.org/TR/DOM-Level-3-Core/core.html#ID-84CF096
; A xml:named-node-map containing the attributes of this node (if it is an
; xml:element) or null otherwise.

; Parameters
; self: The node which provides the function

; Return Value
;   The xml:named-node-map containing the attributes

(define xml:attributes (lambda (self)
  (assert (xml:node? self)
          report: '@xml:attributes)
  (case (xml:nodeTypeSymbol self)
    ((ELEMENT_NODE)
      (element:attributes self))
    (else (node:attributes self)))))


(define xml:copy-node (lambda (self)
  (assert (xml:node? self)
          report: '@xml:copy-node)
  (case (xml:nodeTypeSymbol self)
    ((ELEMENT_NODE)
      (element:copy-node self))
    ((ATTRIBUTE_NODE)
      (attribute:copy-node self))
    ((COMMENT_NODE)
      (comment:copy-node self))
    ((CDATA_SECTION_NODE)
      (cdata:copy-node self))
    ((TEXT_NODE)
      (text:copy-node self))
    (else
      (node:copy-node self)))))

; see: https://www.w3.org/TR/DOM-Level-3-Core/core.html#ID-3A0ED0A4
; Returns a duplicate of this node, i.e., serves as a generic copy constructor
; for nodes. The duplicate node has no parent (parentNode is null) and no user
; data. User data associated to the imported node is not carried over.
; Cloning an xml:element copies all attributes and their values, but this
; method does not copy any children it contains unless it is a deep clone.
; This includes text contained in an the xml:element since the text is
; contained in a child xml:text node. Cloning an xml:attribute directly, as
; opposed to be cloned as part of an xml:element cloning operation, returns a
; specified attribute (specified is true). Cloning an xml:attribute always
; clones its children, since they represent its value, no matter whether this
; is a deep clone or not. Cloning an xml:entity-reference automatically
; constructs its subtree if a corresponding xml:entity is available, no matter
; whether this is a deep clone or not. Cloning any other type of node simply
; returns a copy of this node.
; Note that cloning an immutable subtree results in a mutable copy, but the
; children of an xml:entity-reference clone are readonly.
;
; Parameters
; self: The node which provides the function
; deep: of type boolean
;   If true, recursively clone the subtree under the specified node; if false,;
;   clone only the node itself (and its attributes, if it is an Element).
; Return Value
;   The duplicate node.
(define xml:cloneNode (lambda (self deep)
  (assert (xml:node? self)
          (boolean? deep)
          report: '@xml:cloneNode)
  (case (xml:nodeTypeSymbol self)
    ((TEXT_NODE CDATA_SECTION_NODE COMMENT_NODE)
      (character-data:cloneNode self deep))
    ((ATTRIBUTE_NODE)
     (attribute:cloneNode self deep))
  (else (node:cloneNode self deep)))))

; see: https://www.w3.org/TR/DOM-Level-3-Core/core.html#ID-ElHasAttr
;
; Parameters
; self:
;   The node which provides the function
; name: of type string
;   The name of the attribute to look for.
;
; Return Value
;   true if an attribute with the given name is specified on this element
;   false otherwise.
(define xml:hasAttribute (lambda (self)
  (assert (xml:node? self)
          report: '@xml:hasAttribute)
  (case (xml:nodeTypeSymbol self)
    ((ELEMENT_NODE)
     (element:hasAttribute self))
  (else (node:hasAttribute self)))))

; see: https://www.w3.org/TR/DOM-Level-3-Core/core.html#ID-NodeHasAttrs
; Returns whether this node (if it is an element) has any attributes.
;
; Parameters
; self: The node which provides the function
;
; Return Value
;   true if this node has any attributes, false otherwise.
(define xml:hasAttributes (lambda (self)
  (assert (xml:node? self)
          report: '@xml:hasAttributes)
  (not-empty? (xml:attributes self))))

; see: https://www.w3.org/TR/DOM-Level-3-Core/core.html#ID-810594187
; Returns whether this node has any children.
;
; Parameters
; self: The node which provides the function
;
; Return Value
;   true if this node has any children, false otherwise.
(define xml:hasChildNodes (lambda (self)
  (assert (xml:node? self)
          report: '@xml:hasChildNodes)
  (not (null? (xml:childNodes self)))))

; see: https://www.w3.org/TR/DOM-Level-3-Core/core.html#ID-A6C9094
; Returns a list of all the xml:element's in document order with a given tag
; name and are contained in the document.
;
; Parameters
; self: The node which provides the function
; tagname: of type basic-value
;   The name of the tag to match on. The tagname parameter is case-sensitive
;
; Return Value
;   A new list containing all the matched xml:element's.
(define xml:getElementsByTagName (lambda (self tag-name)
  (assert (xml:node? self)
          (basic-value? tag-name)
          report: '@xml:getElementsByTagName)
  (case (xml:nodeTypeSymbol self)
    ((DOCUMENT_NODE)
      (document:getElementsByTagName self tag-name))
    ((ELEMENT_NODE)
      (element:getElementsByTagName self tag-name))
    (else
      (node:getElementsByTagName self tag-name)))))

; see: https://www.w3.org/TR/DOM-Level-3-Core/core.html#Node3-getUserData
; Retrieves the object associated to a key on a this node. The object must
; first have been set to this node by calling setUserData with the same key.

; Parameters
; self: The node which provides the function
; key of type basic-value
;   The key the object is associated to.

; Return Value
;   the UserData associated to the given key on this node, or null if  there
;   was none.
(define xml:getUserData (lambda (self key)
  (assert (xml:node? self)
          (basic-value? key)
          report: '@xml:getUserData)
  (node:getUserData self key)))

; see: https://www.w3.org/TR/DOM-Level-3-Core/core.html#ID-169727388
;
; Parameters
; self: The node which provides the function
;
; Return
;   The first child of this node. If there is no such node, this returns null.

(define xml:firstChild (lambda (self)
  (assert (xml:node? self)
          report: '@xml:firstChild)
  (node:firstChild self)))

;; This (not DOM function insert tabs and newlines in xml, in order to get a
;; formatted document)
;; Precondition is an unformatted document, without whitespace text elements

(define xml:format (lambda (self depth)
  (assert (xml:node? self)
          (integer? depth)
          report: '@xml:format)
  (case (xml:nodeTypeSymbol self)
    ((ELEMENT_NODE)
     (element:format self depth))
    (else
    (xml:format self depth)))))

; see: https://www.w3.org/TR/DOM-Level-3-Core/core.html#ID-952280727
; Inserts the node newChild before the existing child node refChild. If refChild is null,
; insert newChild at the end of the list of children.
; Parameters
; newChild: of type xml:node
; The node to insert.
; refChild of type xml:node
; The reference node, i.e., the node before which the new node must be inserted.
; Return Value
;
; The node being inserted.
(define xml:insertBefore (lambda (self new-child ref-child)
  (assert (xml:node? self)
          (xml:node? new-child)
          (or (null? ref-child)(xml:node? ref-child))
          report: '@xml:insertBefore)
  (node:insertBefore self new-child ref-child)))

; see: https://www.w3.org/TR/DOM-Level-3-Core/core.html#ID-NodeNSLocalN
; Returns the local part of the qualified name of this node.
; For nodes of any type other than ELEMENT_NODE and ATTRIBUTE_NODE and nodes
; created with a DOM Level 1 method, such as Document.createElement(), this is
; always null.
(define xml:localName (lambda (self)
  (assert (xml:node? self)
          report: '@xml:localName)
  (case (xml:nodeTypeSymbol self)
    ((ELEMENT_NODE)
      (element:localName self))
    ((ATTRIBUTE_NODE)
      (attribute:localName self))
    (else node:localName self))))

; see: https://www.w3.org/TR/DOM-Level-3-Core/core.html#ID-61AD09FB
;
; Parameters
; self: The node which provides the function
;
; Return
;   The last child of this node. If there is no such node, this returns null.
(define xml:lastChild (lambda (self)
  (assert (xml:node? self)
          report: '@xml:lastChild)
  (node:lastChild self)))

; see: https://www.w3.org/TR/DOM-Level-3-Core/core.html#ID-NodeNSPrefix
; as namespace support is not (yet) implemented always return null
(define xml:prefix (lambda (self)
  (assert (xml:node? self)
          report: '@xml:prefix)
  empty))

; see:  https://www.w3.org/TR/DOM-Level-3-Core/core.html#ID-F68D095
;
; Parameters
; self: The node which provides the function
;
; Return
; The name of this node, depending on its type
(define xml:nodeName (lambda (self)
  (assert (xml:node? self)
          report: '@xml:nodeName)
  (case (xml:nodeTypeSymbol self)
    ((CDATA_SECTION_NODE)
      "#cdata-section")
    ((COMMENT_NODE)
      "#comment")
    ((DOCUMENT_NODE)
      "#document")
    ((DOCUMENT_FRAGMENT_NODE)
      "#document-fragment")
    ((TEXT_NODE)
      "#text")
    ((ENTITY_REFERENCE_NODE)
      ;; todo: should return the name of the referenced entity
      "???")
    ((PROCESSING_INSTRUCTION_NODE)
      (xml:target self))
    ((DOCUMENT_TYPE_NODE ENTITY_NODE NOTATION_NODE)
      (xml:name self))
    ((ELEMENT_NODE ATTRIBUTE_NODE)
      (xml:tag self))
    (else (error "unknown node-type for node-name")))))

; see: https://www.w3.org/TR/DOM-Level-3-Core/core.html#ID-111237558
;
; Parameters
; self: The node which provides the function
;
; Return
;   A code representing the type of the underlying object.
;   see table NodeTypes for possible type definitions
(define xml:nodeType (lambda (self)
  (assert (xml:node? self)
          report: '@xml:nodeType)
  (node:nodeType self)))


; see https://www.w3.org/TR/DOM-Level-3-Core/core.html#ID-F68D080
;
; Parameters
; self: The node which provides the function
;
; Return
;   The value of this node, depending on its type
(define xml:nodeValue (lambda (self)
  (assert (xml:node? self)
          report: '@xml:nodeValue)
  (case (xml:nodeTypeSymbol self)
    ((TEXT_NODE CDATA_SECTION_NODE COMMENT_NODE)
      (character-data:nodeValue self))
    ((PROCESSING_INSTRUCTION_NODE)
     (processing-instruction:nodeValue self))
    ((ATTRIBUTE_NODE)
      (attribute:nodeValue self))
    (else (node:nodeValue self)))))

;; no implementation (yet)
(define xml:normalize (lambda (self)
  (raise-dom-exception 'NOT_SUPPORTED_ERR)))

; see: https://www.w3.org/TR/DOM-Level-3-Core/core.html#ID-1734834066
; Removes the child node indicated by oldChild from the list of children, and
; returns it.

; Parameters
; self: The node which provides the function
; oldChild: of type xml:node
;   The node being removed.

; Return Value
;   The node removed.
(define xml:removeChild (lambda (self old-child)
  (assert (xml:node? self)
          (xml:node? old-child)
          report: '@xml:removeChild)
  (node:removeChild self old-child)))

; see: https://www.w3.org/TR/DOM-Level-3-Core/core.html#ID-785887307
; Replaces the child node oldChild with newChild in the list of children, and
; returns the oldChild node. If the newChild is already in the tree, it is
; first removed.

; Parameters
; newChild of type xml:node
;   The new node to put in the child list.
; oldChild of type xml:node
;   The node being replaced in the list.
; Return Value
;   The node replaced.
(define xml:replaceChild (lambda (self new-child old-child)
  (assert (xml:node? self)
          (xml:node? old-child)
          (xml:node? new-child)
          report: '@xml:replaceChild)
  (node:replaceChild self new-child old-child)))

; see:
; https://www.w3.org/TR/DOM-Level-3-Core/core.html#Node3-setUserData
; Associate an object to a key on this node. The object can later be
; retrieved from this node by calling getUserData with the same key.

; Parameters:
; key: of type DOMString
;   The key to associate the object to.
; value: of type DOMUserData
;   The object to associate to the given key, or null to remove any existing
;   association to that key.

; DOM3 also uses an additional argument handler of type UserDataHandler
; which is not (yet) implemented here
(define xml:setUserData (lambda (self key value)
  (assert (xml:node? self)
          (basic-value? key)
          report: '@xml:setUserData)
  (node:setUserData self key value)))

; write node as text to output port
(define xml:write (lambda (self port)
  (assert (xml:node? self)
          (output-port? port)
          report: '@xml:write)
  (case (xml:nodeTypeSymbol self)
    ((DOCUMENT_NODE)
      (document:write self port))
    ((DOCUMENT_TYPE_NODE)
      (document-type:write self port))
    ((ELEMENT_NODE)
      (element:write self port))
    ((ATTRIBUTE_NODE)
      (attribute:write self port))
    ((CDATA_SECTION_NODE)
      (cdata:write self port))
    ((COMMENT_NODE)
      (comment:write self port))
    ((NOTATION_NODE TEXT_NODE)
      (character-data:write self port))
    (else (node:write self port)))))

;; list of forbidden characters in xml, which their xml equivalent
(define xml-entities-alist
  `(
    (#\" . , "&quot;")
    (#\< . , "&lt;")
    (#\> . , "&gt;")
    (#\' . , "&apos;")
    (#\& . , "&amp;")))

; Escapes the characters in a string using XML entities.
; For example: "bread" & "butter" => &quot;bread&quot; &amp; &quot;butter&quot;
; basic-values that are not strings, will be returned as string

; Parameters:
; value: of type basic-value
;   the item to escape

; Return
;   an escaped string
(define escape (lambda (value)
  (assert (basic-value? value)
          report: '@escape)
  (cond ((integer? value) (number->string value))
      ((symbol? value) (escape (symbol->string value)))
      (string? value
        (let* ((escape-character (lambda (ch)
              (let* ((escape (assoc ch  xml-entities-alist)))
              (cond ((pair? escape) (cdr escape))
                (else (string ch)))))))
        (apply string-append (map escape-character (string->list value)))))
      (else (escape (call-with-output-string
                (lambda (port) (write value port))))))))

; Implementation of the xml:node as defined in
; https://www.w3.org/TR/DOM-Level-2-Core/core.html#ID-1950641247 :
; The xml:node class is the base datatype for the entire Document Object
; Model. It represents a single node in the document tree. While all objects
; implementing the xml:node class expose methods for dealing with children,
; not all objects implementing the xml:node class may have children. For
; example, Text nodes may not have children, and adding children to such
; nodes results in a DOMException being raised.
;
; The attributes nodeName, nodeValue and attributes are included as a
; mechanism to get at node information without casting down to the specific
; derived interface. In cases where there is no obvious mapping of these
; attributes for a specific nodeType (e.g., nodeValue for an Element or
; attributes for a Comment), this returns null.

(define-class <xml:node> ()
; parentNode as described in
; https://www.w3.org/TR/DOM-Level-3-Core/core.html#ID-1060184317
; The parent of this node. All nodes, except Attr, Document, DocumentFragment,
; Entity, and Notation may have a parent. However, if a node has just been
; created and not yet added to the tree, or if it has been removed from the
; tree, this is null.
(parentNode #:init-value empty #:accessor xml:parentNode
  #:setter xml:set-parent! #:init-keyword #:parent #:type <xml:node>)
; nodeTypeSymbol is a way to implement nodeType.
; because it is better readable when using symbols,
; the decision  is made to use a symbol in the code, and return a integer
; value if the xml:nodeType() method is called
;
; symbol should be one of:
; ELEMENT_NODE ATTRIBUTE_NODE TEXT_NODE CDATA_SECTION_NODE
; ENTITY_REFERENCE_NODE ENTITY_NODE PROCESSING_INSTRUCTION_NODE COMMENT_NODE
; DOCUMENT_NODE DOCUMENT_TYPE_NODE DOCUMENT_FRAGMENT_NODE NOTATION_NODE

  ;; no initial value, should be filled in constructor
  (nodeTypeSymbol #:accessor xml:nodeTypeSymbol #:init-keyword #:symbol
    #:type <symbol>)
; ownerDocument as described in
; https://www.w3.org/TR/DOM-Level-3-Core/core.html#node-ownerDoc
; The Document object associated with this node. This is also the Document
; object used to create new nodes. When this node is a Document or a
; DocumentType which is not used with any Document yet, this is null.
  (ownerDocument #:init-value empty #:accessor xml:ownerDocument
    #:setter xml:set-owner-document! #:init-keyword #:owner-document
    #:type <xml:document>)

; see: https://www.w3.org/TR/DOM-Level-3-Core/core.html#ID-1451460987
; A list that contains all children of this node.
  (childNodes #:init-value empty #:accessor xml:childNodes
    #:setter xml:set-child-nodes! #:init-keyword #:child-nodes #:type <list>)

; for speedup operations there is a permanent housekeeping of previous sibling
; see: https://www.w3.org/TR/DOM-Level-3-Core/core.html#ID-640FB3C8
;
; xml:previousSibling
; Parameters
; self: The node which provides the function
;
; Return
;   The node immediately preceding this node. If there is no such node, this
;   returns null.

  (previousSibling #:init-value empty #:accessor xml:previousSibling
    #:setter xml:set-previousSibling! #:init-keyword #:previousSibling
    #:type <xml:node>)

; for speedup operations there is a permanent housekeeping of next sibling
; see: https://www.w3.org/TR/DOM-Level-3-Core/core.html#ID-6AC54C2F

; xml:nextSibling
; Parameters
; self: The node which provides the function
;
; Return
;   The node immediately following this node. If there is no such node, this
;   returns null.
  (nextSibling #:init-value empty #:accessor xml:nextSibling
    #:setter xml:set-nextSibling! #:init-keyword #:nextSibling
    #:type <xml:node>)
; a key value list of userdata
; The DOMUserData type is used to store application data.
  (userdata-collection #:init-value '() #:accessor xml:userdata-collection
    #:setter xml:setUserData-collection! #:init-keyword #:userdata-collection
      #:type <list>)
)

; see description of xml:appendChild
(define node:appendChild (lambda (self new-child)
  (assert (xml:node? self)
          (xml:node? new-child)
          report: '@node:appendChild)
;; todo check for same owner
  (letrec ((lst (xml:childNodes self))
    (item-append (lambda (self lst i)
        (cond ((null? lst)
            (xml:set-parent! i self)
            (cons i '()))
          ((null? (cdr lst))
            (xml:set-nextSibling! (car lst) i)
            (xml:set-previousSibling! i (car lst))
            (xml:set-parent! i self)
            (cons (car lst) (item-append self (cdr lst) i)))
          (else
            (cons (car lst) (item-append self (cdr lst) i)))))))
    (xml:set-child-nodes! self (item-append self lst new-child))
    new-child)))

; see description of xml:attributes
(define node:attributes (lambda (self)
  (assert (xml:node? self)
          report: '@node:attributes)
  empty))

; see description of xml:copy-node
(define node:copy-node (lambda (self)
  (assert (xml:node? self)
          report: '@node:copy-node)
  (raise-dom-exception 'NOT_SUPPORTED_ERR)))

(define node:cloneNode (lambda (self deep)
  (assert (xml:node? self)
          (boolean? deep)
          report: '@node:cloneNode)
  (let* ((result (xml:copy-node self))
      (lst (xml:childNodes self)))
    (cond (deep
        (for-each (lambda (n) (xml:appendChild result (xml:cloneNode n deep))) lst)))
    result)))


; see description of xml:firstChild
(define node:firstChild (lambda (self)
  (assert (xml:node? self)
          report: '@node:firstChild)
  (let* ((lst (xml:childNodes self)))
    (if (null? lst)'()(car lst)))))

; see description of xml:getElementsByTagName
(define node:getElementsByTagName (lambda (self tag-name)
  (assert (xml:node? self)
          (basic-value? tag-name)
          report: '@node:getElementsByTagName)
  (raise-dom-exception 'INVALID_ACCESS_ERR)))

; see description of xml:getUserData
(define node:getUserData (lambda (self key)
  (assert (xml:node? self)
          (basic-value? key)
          report: '@node:getUserData)
  (let* ((lst (xml:userdata-collection self))
      (res (assq key lst)))
      (if (pair? res) (cdr res) empty))))

; see description of xml:format
(define node:format
 (lambda (self depth)
  (assert (xml:node? self)
          (integer? depth)
          report: '@node:format)
 ))

; see description of xml:hasAttribute
(define xml:hasAttribute (lambda (self)
  (assert (xml:node? self)
          report: '@node:hasAttribute)
  (raise-dom-exception 'INVALID_ACCESS_ERR)))

; see description of xml:insertBefore
(define node:insertBefore (lambda (self new-child ref-child)
  (assert (xml:node? self)
          (xml:node? new-child)
          (or (null? ref-child)(xml:node? ref-child))
          report: '@node:insertBefore)
  (if (null? ref-child)
    (node:appendChild self new-child)
    (letrec ((lst (xml:childNodes self))
        (insertBefore (lambda (self lst n r)
            (cond
              ((null? lst) (raise-dom-exception 'NOT_FOUND_ERR))
              ((eq? (car lst) r)
                (let* ((prev (xml:previousSibling r))
                    (next r))
                  (cond ((not (null? prev))
                    (xml:set-nextSibling! prev n)))
                  (xml:set-previousSibling! n prev)
                  (xml:set-nextSibling! n next)
                  (xml:set-previousSibling! next n)
                  (xml:set-parent! n self)
                  (cons n lst)))
              (else (cons (car lst) (insertBefore self (cdr lst) n r)))))))
      (let ((new-lst (insertBefore self lst new-child ref-child)))
        (cond ((null? new-lst) empty)
          (else
            (xml:set-child-nodes! self new-lst)
            new-child)))))))

; see description of xml:localName
(define node:localName (lambda (self)
  (assert (xml:node? self)
          report: '@node:localName)
  empty))

; see description of xml:lastChild
(define node:lastChild (lambda (self)
  (assert (xml:node? self)
          report: '@node:lastChild)
  (letrec ((lst (xml:childNodes self))
      (last (lambda (lst)
        (cond ((null? lst) empty )
          ((null? (cdr lst)) (car lst))
          (else
            (last (cdr lst)))))))
    (last lst))))

; see description of xml:nodeValue
(define node:nodeValue (lambda (self)
  (assert (xml:node? self)
          report: '@node:nodeValue)
  empty ))

; see description of xml:nodeType
(define node:nodeType (lambda (self)
  (assert (xml:node? self)
          report: '@node:nodeType)
  (let* ((lst NodeTypes))
    (let* ((res (assoc (xml:nodeTypeSymbol self) lst)))
      (cond ((pair? res) (cdr res))
        (else (error ("node with unknown type"))))))))

; see description of xml:removeChild
(define node:removeChild (lambda (self old-child)
  (assert (xml:node? self)
          (xml:node? old-child)
          report: '@node:removeChild)
  (letrec ((lst (xml:childNodes self))
      (remove-child (lambda (lst o)
        (cond
          ((null? lst) (raise-dom-exception 'NOT_FOUND_ERR))
          ((equal? (car lst) o)
            (let* ((prev (xml:previousSibling o))
              (next (xml:nextSibling o)))
              (if (not (null? next)) (xml:set-previousSibling! next prev) '())
              (if (not (null? prev)) (xml:set-nextSibling! prev next) '())
              (xml:set-parent! o empty)
              (cdr lst)))
          (else (cons (car lst) (remove-child (cdr lst) o)))))))
    (let ((new-lst (remove-child lst old-child)))
      (xml:set-child-nodes! self new-lst)
      old-child))))

; see description of xml:replaceChild
(define node:replaceChild (lambda (self new-child old-child)
  (assert (xml:node? self)
          (xml:node? old-child)
          (xml:node? new-child)
          report: '@node:replaceChild)
  (letrec ((lst (xml:childNodes self))
      (replace-child (lambda (self lst n o)
          (cond
            ((null? lst) (raise-dom-exception 'NOT_FOUND_ERR))
            ((eq? (car lst) o)
              (let* ((prev (xml:previousSibling o))
                (next (xml:nextSibling o)))
                (xml:set-nextSibling! o empty)
                (xml:set-previousSibling! o empty)
                (xml:set-parent! o empty)
                (xml:set-nextSibling! n next)
                (xml:set-previousSibling! n prev)
                (xml:set-parent! n self)
                (cons n (cdr lst))))
            (else (cons (car lst) (replace-child self (cdr lst) n o)))))))
    (let ((new-lst (replace-child self lst new-child old-child)))
      (cond ((null? new-lst) empty) ;; do nothing as exception already raised
        (else
          (xml:set-child-nodes! self new-lst)
          old-child))))))

; see description of xml:setUserData
(define node:setUserData (lambda (self key value)
  (assert (xml:node? self)
          (basic-value? key)
          report: '@node:setUserData)
  (let ((lst (xml:userdata-collection self)))
    (cond ((null? lst)
            (xml:setUserData-collection! self (acons key value '())))
      (else (xml:setUserData-collection! self (assoc-set! lst key value)))))))

; see description of xml:write
(define node:write (lambda (self port)
  (assert (xml:node? self)
          (output-port? port)
          report: '@node:write)
    (for-each (lambda (n) (xml:write n port)) (xml:childNodes self))))

; see: https://www.w3.org/TR/DOM-Level-3-Core/core.html#ID-FF21A306
; The xml:character-data class extends xml:node with methods for accessing
; character data in the DOM. For clarity this set is defined here rather than
; on each object that uses these attributes and methods. No DOM objects
; correspond directly to CharacterData, though Text and others do inherit the
; interface from it. All offsets in this interface start from 0.
(define-class <xml:character-data> (<xml:node>)
  (character-data #:accessor xml:data #:setter xml:set-character-data!
  #:init-keyword #:character-data)
  (whitespace #:init-value #f #:accessor whitespace?
  #:init-keyword #:whitespace)
)

; see description of xml:nodeValue
(define character-data:nodeValue (lambda (self)
  (assert (xml:character-data? self)
          report: '@character-data:nodeValue)
  (xml:data self)))

; see:
; https://www.w3.org/TR/DOM-Level-3-Core/core.html#Text3-isElementContentWhitespace

; isElementContentWhitespace
; Returns whether this text node contains element content whitespace, often
; abusively called "ignorable whitespace". The text node is determined to
; contain whitespace in element content during the load of the document or if
; validation occurs while using Document.normalizeDocument().
;
; Parameters
; self: The node which provides the function
;
; Return Value
;   true if this node contains white space as described, false otherwise.
(define xml:isElementContentWhitespace (lambda (self)
  (assert (xml:character-data? self)
          report: '@character-data:isElementContentWhitespace)
  (whitespace? self)))

; see description of xml:cloneNode
(define character-data:cloneNode (lambda (self deep)
  (assert (xml:character-data? self)
          (boolean? deep)
          report: '@character-data:cloneNode)
  (let* ((result (xml:copy-node self))
    (lst (xml:childNodes self)))
      (for-each (lambda (n) (xml:appendChild result (xml:cloneNode n #t))) lst)
    result)))

; see description of xml:write
(define character-data:write (lambda (self port)
  (assert (xml:character-data? self)
          (output-port? port)
          report: '@character-data:write)
  (display (escape (xml:data self)) port)))


; The xml:text class inherits from xml:character-data and represents the
; textual content (termed character data in XML) of an xml:element or
; xml:attribute.
; No lexical check is done on the content of a xml:text node and, depending on
; its position in the document, some characters must be escaped during
; serialization using character references; e.g. the characters "<&" if the
; textual content is part of an element or of an attribute, the character
; sequence "]]>" when part of an element, the quotation mark character " or the
; apostrophe character ' when part of an attribute.
(define-class <xml:text> (<xml:character-data>)
)

(define make-text (lambda (content owner-document)
  (assert (basic-value? content)
          (xml:document? owner-document)
          report: '@make-text)
  (make <xml:text>
    #:symbol 'TEXT_NODE
    #:character-data content
    #:owner-document owner-document)))

(define text:copy-node (lambda (self)
  (assert (xml:text? self)
          report: '@text:copy-node)
  (make <xml:text>
    #:symbol 'TEXT_NODE
    #:whitespace (whitespace? self)
    #:character-data (xml:data self)
    #:owner-document (xml:ownerDocument self))))

;; todo: check for non-whitespace in content?
(define make-white-space (lambda (content owner-document)
  (assert (string? content)
          (xml:document? owner-document)
          report: '@make-white-space)
  (make <xml:text>
    #:whitespace #t
    #:symbol 'TEXT_NODE
    #:character-data content
    #:owner-document owner-document )))

(define-class <xml:cdata> (<xml:character-data>)
)

(define make-cdata (lambda (content owner-document)
  (assert (string? content)
          (xml:document? owner-document)
          report: '@make-cdata)
  (make <xml:cdata>
    #:symbol 'CDATA_SECTION_NODE
    #:character-data content
    #:owner-document owner-document)))

(define cdata:copy-node (lambda (self)
  (assert (xml:cdata? self)
          report: '@cdata:copy-node)
  (make <xml:cdata>
      #:symbol 'CDATA_SECTION_NODE
      #:whitespace (whitespace? self)
      #:character-data (xml:data self)
      #:owner-document (xml:ownerDocument self))))

(define cdata:write (lambda (self  port)
  (assert (xml:cdata? self)
          (output-port? port)
          report: '@cdata:write)
  (display "<![CDATA[" port)
  (display (xml:data self) port)
  (display "]]>" port)))

(define-class <xml:comment> (<xml:character-data>)
)

(define make-comment (lambda (content owner-document)
  (assert (string? content)
          (xml:document? owner-document)
          report: '@make-comment)
  (make <xml:comment>
    #:symbol 'COMMENT_NODE
    #:character-data content
    #:owner-document owner-document )))

(define comment:copy-node (lambda (self)
  (assert (xml:comment? self)
          report: '@comment:copy-node)
  (make <xml:comment>
      #:symbol 'COMMENT_NODE
      #:whitespace (whitespace? self)
      #:character-data (xml:data self)
      #:owner-document (xml:ownerDocument self))))

(define comment:write (lambda (self port)
  (assert (xml:comment? self)
          (output-port? port)
          report: '@comment:write)
  (display "<!--" port)
  (display (xml:data self) port)
  (display "-->" port)))

(define-class <xml:named-node> (<xml:node>)
)

(define named-node-map:write (lambda (self port)
  (assert (xml:named-node? self)
          (output-port? port)
          report: '@named-node:write)
  (cond ((xml:attribute? self)
      (attribute:write self port))
    (else
      (display " " port)
      (display (xml:nodeName self) port)
      (display "=\"" port)
      (display (escape (xml:nodeValue self)) port)
      (display "\"" port)))))

(define-class <xml:attribute> (<xml:named-node>)
  (tag #:init-value empty #:accessor xml:tag
  #:setter xml:set-tag! #:init-keyword #:tag)
)

(define make-attribute-value (lambda (name owner-document value)
  (assert (basic-value? name)
          (xml:document? owner-document)
          (or (null? value)(basic-value? value))
          report: '@make-attribute-value)
  (let ((self (make <xml:attribute>
        #:symbol 'ATTRIBUTE_NODE
        #:tag name
        #:owner-document owner-document
        )))
    (if (null? value)
      '()
      (attribute:set-value! self value)
    )
    self)))

(define make-attribute (lambda (name owner-document)
  (assert (basic-value? name)
          (xml:document? owner-document)
          report: '@make-attribute-value)
  (make-attribute-value name owner-document '())))

(define attribute:copy-node (lambda (from)
  (assert (xml:attribute? from)
          report: '@attribute:copy-node)
  (make <xml:attribute>
          #:symbol 'ATTRIBUTE_NODE
          #:tag (xml:tag from)
          #:owner-document (xml:ownerDocument from))))

(define attribute:set-value! (lambda (self value)
  (assert (xml:attribute? self)
          (or (null? value)(basic-value? value))
          report: '@attribute:set-value!)
  (let ((node (make-text value (xml:ownerDocument self))))
    (xml:set-child-nodes! self (cons node '())))))


(define attribute:cloneNode (lambda (self deep)
  (assert (xml:attribute? self)
          (boolean? deep)
          report: '@attribute:cloneNode)
  (let* ((result (attribute:copy-node self))
      (lst (xml:childNodes self)))
        (for-each (lambda (n) (xml:appendChild result (xml:cloneNode n #t))) lst)
    result)))

(define attribute:localName (lambda (self)
  (assert (xml:attribute? self)
          report: '@attribute:localName)
  (xml:tag self)))

(define attribute:nodeValue (lambda (self)
  (assert (xml:attribute? self)
          report: '@attribute:nodeValue)
  (xml:value self)))

; On retrieval, the value of the attribute is returned as a string. Character
; and general entity references are replaced with their values. See also the
; method xml:getAttribute of the xml:element class.
; On setting, this creates a Text node with the unparsed contents of the
; string, i.e. any characters that an XML processor would recognize as markup
; are instead treated as literal text. See also the method xml:setAttribute().
(define*  (xml:value self #:optional (value '()))
  (assert (xml:attribute? self)
          (or (null? value) (basic-value? value))
          report: '@xml:value)
  (if (null? value)
    (if (xml:hasChildNodes self)
      (let ((result '()))
        (for-each
          (lambda (node)
            (cond
              ((and
                (xml:text? node)
                (not (xml:isElementContentWhitespace node)))
              (cond
                ((null? result)
                  (set! result (xml:nodeValue node)))
                (else
                  (set! result (format #f "~s~s"
                          (xml:nodeValue node) result)))))))
        (xml:childNodes self))
      result)
      empty)
    (let ((node (make-text value (xml:ownerDocument self))))
      (xml:set-child-nodes! self (cons node '())))))

(define attribute:write (lambda (self port)
  (assert (xml:attribute? self)
          (output-port? port)
          report: '@attribute:write)
  (display " " port)
  (display (xml:tag self) port)
  (display "=\"" port)
  (display (escape (xml:value self)) port)
  (display #\" port)))

; The xml:named-node-map class represent collections of nodes that can be
; accessed by name.
; it is implemented as an association list of (name . node) pairs,
; where  name is equal to (xml:nodeName node) NamedNodeMaps are not
; maintained in any particular order.
(define-class <xml:named-node-map> ()
  (named-nodes #:init-value '() #:accessor xml:named-nodes
    #:setter xml:set-named-nodes! #:init-keyword #:named-nodes
    #:type <list>)
)

(define named-node-map:write (lambda (self port)
  (assert (xml:named-node-map? self)
          (output-port? port)
          report: '@named-node-map:write)
  (for-each
    (lambda (n)(xml:write (cdr n) port))
    (xml:named-nodes self))))

; see: https://www.w3.org/TR/DOM-Level-3-Core/core.html#ID-1074577549
; Retrieves a node specified by name.

; Parameters
; self: of type xml:named-node-map
;   the named-node-map on which the operation is performed
; name: of type basic-value
;   The nodeName of a node to retrieve.
; Return Value
;   A xml:node (of any type) with the specified nodeName, or null if it does
;   not identify any node in this map.
(define xml:getNamedItem (lambda (self name)
  (assert (xml:named-node-map? self)
          (basic-value? name)
          report: '@xml:getNamedItem)
  (let *((lst (xml:named-nodes self)))
    (cond ((null? lst) #f)
      (else
        (let* ((res (assoc name lst)))
          (cond ((pair? res) (cdr res))
            (else #f))))))))

(define named-node-map:has-named-item (lambda (self name)
  (assert (xml:named-node-map? self)
          (basic-value? name)
          report: '@named-node-map:has-named-item)
  (let *((lst (xml:named-nodes self)))
    (cond ((null? lst) #f)
      (else
        (let* ((res (assoc name lst)))
          (cond ((pair? res) #t)
            (else #f))))))))

; see: https://www.w3.org/TR/DOM-Level-3-Core/core.html#ID-1025163788
; Adds a node using its nodeName attribute. If a node with that name is
; already present in this map, it is replaced by the new one. Replacing a node
; by itself has no effect.
; As the nodeName attribute is used to derive the name which the node must be
; stored under, multiple nodes of certain types (those that have a "special"
; string value) cannot be stored as the names would clash. This is seen as
; preferable to allowing nodes to be aliased.

; Parameters
; self: of type xml:named-node-map
;   the named-node-map on which the operation is performed
; node: of type xml:node
;   A node to store in this map. The node will later be accessible using the
;   value of its nodeName attribute.
; Return Value
;   If the new xml:node replaces an existing node the replaced Node is
;   returned, otherwise null is returned.
(define xml:setNamedItem (lambda (self node)
  (assert (xml:named-node-map? self)
          (xml:node? node)
          report: '@xml:setNamedItem)
  (let* ((lst (xml:named-nodes self)))
    (xml:set-named-nodes! self (assoc-set! lst (xml:nodeName node) node)))))

(define make-named-node-map (lambda ()
  (make <xml:named-node-map>)))

(define make-named-node-map-from (lambda (from)
  (assert (xml:named-node-map? from)
          report: '@make-named-node-map-from)
  (let ((result (make-named-node-map))
      (lst (xml:named-nodes from)))
    (for-each
      (lambda (n)
        (cond ((pair? n)
            (xml:setNamedItem result (xml:cloneNode (cdr n) #f)))))
      lst))))

 (define named-node-map:cloneNodes (lambda (from)
  (assert (xml:named-node-map? from)
          report: '@named-node-map:cloneNodes)
  (let ((result (make-named-node-map))
      (lst (xml:named-nodes from)))
    (for-each
      (lambda (n)
        (cond ((pair? n)
            (xml:setNamedItem result (xml:cloneNode (cdr n) #f)))))
      lst))))

(define named-node-map:has-attribute (lambda (self name)
  (assert (xml:named-node-map? self)
          (basic-value? name)
          report: '@named-node-map:has-attribute)
  (let *((lst (xml:named-nodes self)))
    (if (null? lst)
      #f
      (pair? (assoc name lst))))))

;; we are interested in the value of the attribute node
(define named-node-map:get-attribute (lambda (self name)
  (assert (xml:named-node-map? self)
          (basic-value? name)
          report: '@named-node-map:get-attribute)
  (let ((attribute (xml:getNamedItem self name)))
  (if attribute
    (xml:value attribute)
    '()))))

(define named-node-map:get-attribute-node (lambda (self name)
  (assert (xml:named-node-map? self)
          (basic-value? name)
          report: '@named-node-map:get-attribute-node)
  (xml:getNamedItem self name)))

(define named-node-map:set-attribute
      (lambda (self name value owner-document)
  (assert (xml:named-node-map? self)
          (basic-value? name)
          (basic-value? value)
          (xml:document? owner-document)
          report: '@named-node-map:set-attribute)
  (cond ((named-node-map:has-named-item self name)
      (attribute:set-value! (xml:getNamedItem self name) value))
    (else
      (let ((attr (make-attribute-value name owner-document value)))
        (xml:setNamedItem self attr))))))

; This the xml:entity class represents an entity, either parsed or unparsed,
; in an XML document. Note that this models the entity itself not the entity
; declaration. Entity declaration modeling has been left for a later Level of
; the DOM specification.

; The nodeName attribute that is inherited from Node contains the name of the
; entity.

; An XML processor may choose to completely expand entities before the
; structure model is passed to the DOM; in this case there will be no
; EntityReference nodes in the document tree.

; XML does not mandate that a non-validating XML processor read and process
; entity declarations made in the external subset or declared in external
; parameter entities. This means that parsed entities declared in the external
; subset need not be expanded by some classes of applications, and that the
; replacement value of the entity may not be available. When the replacement
; value is available, the corresponding xml:entity's child list represents the
; structure of that replacement text. Otherwise, the child list is empty.
(define-class <xml:entity> (<xml:node>)
  (publicId #:init-value "" #:accessor xml:publicId
    #:setter xml:set-publicId! #:init-keyword #:publicId #:type <string>)
  (systemId #:init-value "" #:accessor xml:systemId
    #:setter xml:set-systemId! #:init-keyword #:systemId #:type <string>)
  (notationName #:init-value "" #:accessor xml:notationName
    #:setter xml:set-notationName! #:init-keyword #:notationName
    #:type <string>)
  (xmlVersion #:init-value "" #:accessor xml:xmlVersion
    #:setter xml:set-xmlVersion! #:init-keyword #:xmlVersion
    #:type <string>)
  )
;   #:symbol 'ENTITY_NODE


; see: https://www.w3.org/TR/DOM-Level-2-Core/core.html#ID-11C98490
; xml:entity-reference objects may be inserted into the structure model when
; an entity reference is in the source document, or when the user wishes to
; insert an entity reference. Note that character references and references to
; predefined entities are considered to be expanded by the XML processor so
; that characters are represented by their Unicode equivalent rather than by an
; entity reference. Moreover, the XML processor may completely expand
; references to entities while building the structure model, instead of
; providing xml:entity-reference objects. If it does provide such objects, then
; for a given xml:entity-reference node, it may be that there is no xml:entity
; node representing the referenced entity. If such an xml:entity exists, then
; the subtree of the xml:entity-reference node is in general a copy of the
; xml:entity node subtree.

;; todo: implement methods
(define-class <xml:entity-reference> (<xml:node>)
  )
;   #:symbol 'ENTITY_REFERENCE_NODE


; see: https://www.w3.org/TR/DOM-Level-2-Core/core.html#ID-1004215813
; The ProcessingInstruction interface represents a "processing instruction",
; used in XML as a way to keep processor-specific information in the text of
; the document.
;; todo: implement methods
(define-class <xml:processing-instruction> (<xml:node>)
  (target #:init-value "" #:accessor xml:target #:setter xml:set-target!
    #:init-keyword #:target #:type <string>)
  (data #:init-value "" #:accessor xml:data #:setter xml:set-data!
    #:init-keyword #:data #:type <string>)
  )
;   #:symbol 'PROCESSING_INSTRUCTION_NODE

(define processing-instruction:nodeValue (lambda (self)
  (assert (processing-instruction? self)
          report: '@processing-instruction:nodeValue)
  (xml:data self)))

; see: https://www.w3.org/TR/DOM-Level-3-Core/core.html#ID-5431D1B9
; This class represents a notation declared in the DTD. A notation either
; declares, by name, the format of an unparsed entity (see section 4.7 of the
; XML 1.0 specification https://www.w3.org/TR/2004/REC-xml-20040204/#Notations)
; , or is used for formal declaration of processing instruction targets
; (see section 2.6 of the XML 1.0 specification
; https://www.w3.org/TR/2004/REC-xml-20040204/#sec-pi).
; The nodeName attribute inherited from Node is set to the declared name of
; the notation.

; The DOM Core does not support editing Notation nodes;

; A Notation node does not have any parent.
(define-class <xml:notation> (<xml:node>)
  (publicId #:init-value "" #:accessor xml:publicId
    #:setter xml:set-publicId! #:init-keyword #:publicId #:type <string>)
  (systemId #:init-value "" #:accessor xml:systemId
    #:setter xml:set-systemId! #:init-keyword #:systemId #:type <string>)
  )
;   #:symbol 'NOTATION_NODE

; see: https://www.w3.org/TR/DOM-Level-2-Core/core.html#ID-412266927
; Each xml:document has a doctype attribute whose value is either null or a
; xml:document-type object. The xml:document-type class in the DOM Core
; provides an interface to the list of entities that are defined for the
; document
(define-class <xml:document-type> (<xml:node>)
  (name #:init-value "" #:accessor xml:name #:setter xml:set-name!
    #:init-keyword #:tag #:type <string>)
  (entities #:init-value empty #:accessor xml:entities
    #:setter xml:set-entities! #:init-keyword #:entities
    #:type <xml:named-node-map>)
  (notations #:init-value empty #:accessor xml:notations
    #:setter xml:set-notations! #:init-keyword #:notations
    #:type <xml:named-node-map>)
  (publicId #:init-value "" #:accessor xml:publicId
    #:setter xml:set-publicId! #:init-keyword #:publicId
    #:type <string>)
  (systemId #:init-value "" #:accessor xml:systemId
    #:setter xml:set-systemId! #:init-keyword #:systemId
    #:type <string>)
  (internal-subset #:init-value "" #:accessor xml:internal-subset
    #:setter xml:set-internal-subset! #:init-keyword #:internal-subset
    #:type <string>)
)

(define make-document-type (lambda(name public-id system-id)
  (assert (basic-value? name)
          (string? public-id)
          (string? system-id)
          report: '@make-document-type)
  (make <xml:document-type>
    #:symbol 'DOCUMENT_TYPE_NODE
    #:tag name
    #:publicId public-id
    #:systemId system-id )))

(define document-type:write (lambda (self port)
  (assert (xml:document-type? self)
          (output-port? port)
          report: '@document-type:write)
  (display "<!DOCTYPE " port)
  (display (xml:name self) port)
  (if (or (null? (xml:publicId self)) (equal? (xml:publicId self) ""))
    (display " SYSTEM \"" port)
    (begin
      (display " PUBLIC \"" port)
      (display (xml:publicId self) port)
      (display "\"  \"" port))
  )
  (display (xml:systemId self) port)
  ;; todo implement writing of entities and notations
  (display "\">\n" port)))

; The xml:element class represents an element in an XML document.
; xml:element's may have attributes associated with them; since the
; xml:element class inherits from xml:node, the generic xml:node class
; attribute 'attributes' may be used to retrieve the set of all attributes for
; an element. There are methods on the  interface to retrieve either an
; xml:attribute object by name or an attribute value by name. In XML, where an
; attribute value may contain entity references, an xml:attribute object
; should be retrieved to examine the possibly fairly complex sub-tree
; representing the attribute value.
; On the other hand, when we have knowledge of the contents type, methods
; to directly access an attribute value can safely be used as a convenience.
(define-class <xml:element> (<xml:node>)
  (name #:init-value empty #:accessor xml:tag #:setter xml:set-tag!
    #:init-keyword #:tag )
  (attributes #:init-value empty
        #:accessor element:attributes-collection
        #:setter element:set-attributes!
        #:init-keyword #:attributes
        #:type <xml:named-node-map>)
      )


(define make-element (lambda (tag-name owner-document)
  (assert (basic-value? tag-name)
          (xml:document? owner-document)
          report: '@make-element)
  (make <xml:element>
    #:symbol 'ELEMENT_NODE
    #:tag tag-name
    #:owner-document owner-document
    #:attributes (make-named-node-map)
    )))

(define element:copy-node (lambda (self)
  (assert (xml:element? self)
          report: '@element:copy-node)
  (let ((attr (if (null? (element:attributes self))
      (make-named-node-map)
      (make-named-node-map-from (element:attributes self))
    )))
  (make <xml:element>
    #:symbol 'ELEMENT_NODE
    #:tag (xml:tag self)
    #:owner-document (xml:ownerDocument self)
    #:attributes attr))))

(define element:attributes (lambda (self)
  (assert (xml:element? self)
          report: '@element:attributes)
  (let ((lst (xml:named-nodes (element:attributes-collection self))))
    (cond ((null? lst) '())
      ((list? lst)
        (map cdr lst))
      (else '())))))

(define xml:getAttribute (lambda (self name)
  (assert (xml:element? self)
          (basic-value? name)
          report: '@element:getAttribute)
  (named-node-map:get-attribute
        (element:attributes-collection self) name)))

(define xml:getAttributeNode (lambda (self name)
  (assert (xml:element? self)
          (basic-value? name)
          report: '@element:getAttributeNode)
  (named-node-map:get-attribute-node
        (element:attributes-collection self) name)))

(define element:hasAttribute (lambda (self name)
  (assert (xml:element? self)
          (basic-value? name)
          report: '@element:hasAttribute)
  (named-node-map:has-attribute
        (element:attributes-collection self) name)))

(define element:localName (lambda (self)
  (assert (xml:element? self)
          report: '@element:localName)
  (xml:tag self)))

(define xml:setAttribute (lambda (self name value)
  (assert (xml:element? self)
          (basic-value? name)
          (basic-value? value)
          report: '@element:setAttribute)
  (named-node-map:set-attribute
        (element:attributes-collection self)
        name value (xml:ownerDocument self))))

(define xml:setAttributeNode (lambda (self node)
  (assert (xml:element? self)
          (xml:node? node)
          report: '@element:setAttributeNode)
  (xml:setNamedItem
        (element:attributes-collection self)
        node)))

(define element:write (lambda (self port)
  (assert (xml:element? self)
          (output-port? port)
          report: '@element:write)
  (cond ((xml:hasChildNodes self)
      (display "<" port)
      ;; todo use ns manager from owner
      (display (xml:tag self) port)
      (named-node-map:write (element:attributes-collection self)  port)
      (display ">" port)
      (node:write self port)
      (display "</" port)
      (display (xml:tag self) port)
      (display ">" port)
    )
    (else
      (display "<" port)
        ;; todo use ns manager from owner
      (display (xml:tag self) port)
      (named-node-map:write (element:attributes-collection self) port)
      (display "/>" port)))))

; Parameters
; self: the node on which the operation is performed
;
; Return Value
; The name of the element. If Node.localName is different from null, this
; attribute is a qualified name. For example, in:
;           <elementExample id="demo">
;           ...
;           </elementExample>
; tagName has the value "elementExample". Note that this is case-preserving
; as are all of the operations of the DOM.
(define element:tagName (lambda (self name)
  (assert (xml:element? self)
          (basic-value? name)
          report: '@element:tagName)
  (xml:tag self)))

; see: https://www.w3.org/TR/DOM-Level-3-Core/core.html#ID-1938918D
; Returns a list of all the descendant xml:element's with a given tag name in document order.

; Parameters
; self: the node on which the operation is performed
; name: of type basic-value
;   The name of the elements to match on.
;
; Return Value
; A new list containing all the matched xml:element's.
(define element:getElementsByTagName (lambda (self name)
  (assert (xml:element? self)
          (basic-value? name)
          report: '@element:getElementsByTagName)
  (append
    (apply append (map (lambda (ch)
      (cond ((xml:element? ch)(element:getElementsByTagName ch name))
        (else '()))) (xml:childNodes self)))
    (if (equal? name (xml:tag self)) (list self) '()))))

(define indent-char #\tab )

(define element:format
 (lambda (self depth)
   (assert (xml:element? self)
          (integer? depth)
          report: '@element:format)
  (let ((newline-needed #f))
    (for-each (lambda (node)
      (cond ((xml:element? node)
        (xml:insertBefore self
          (make-white-space
            (string #\newline) (xml:ownerDocument self)) node )
        (xml:insertBefore self
          (make-white-space
              (make-string depth indent-char) (xml:ownerDocument self)) node)
        (set! newline-needed #t)
        (element:format node (1+ depth)))
        (else
          (node:format node (1+ depth)))
        )
      )
    (xml:childNodes self))
    (cond (newline-needed
      (xml:appendChild self (make-white-space (string #\newline)
            (xml:ownerDocument self)))
      (xml:appendChild self
            (make-white-space (make-string (- depth 1) indent-char)
                  (xml:ownerDocument self))))))))

; see https://www.w3.org/TR/DOM-Level-2-Core/core.html#i-Document
; The xml:document class represents the entire XML document.
; Conceptually, it is the root of the document tree, and provides the primary
; access to the document's data.

; Since elements, text nodes, comments, processing instructions, etc. cannot
; exist outside the context of a Document, the Document interface also
; contains the factory methods needed to create these objects. The xml:node
; objects created have a ownerDocument attribute which associates them with
; the xml:document within whose context they were created.
(define-class <xml:document> (<xml:node>)
  (element  #:accessor xml:documentElement
    #:setter document:set-element! #:init-keyword #:element)
  (document-type  #:accessor xml:doctype #:setter document:set-document-type!
    #:init-keyword #:document-type #:type <xml:document-type>)
  (inputEncoding #:init-value '() #:accessor xml:inputEncoding
    #:setter xml:set-inputEncoding! #:init-keyword #:inputEncoding
    #:type <string>)
  (xmlEncoding #:init-value '() #:accessor xml:xmlEncoding
    #:setter xml:set-xmlEncoding! #:init-keyword #:xmlEncoding
    #:type <string>)
  (xmlVersion #:init-value "1.0" #:accessor xml:xmlVersion
    #:setter xml:set-xmlVersion! #:init-keyword #:xmlVersion
    #:type <string>)
  (xmlStandalone #:init-value #f #:accessor xml:xmlStandalone
    #:setter xml:set-xmlStandalone! #:init-keyword #:xmlStandalone
    #:type <boolean>)
)

(define document:write (lambda (self port)
   (assert (xml:document? self)
          (output-port? port)
          report: '@document:write)
  (display "<?xml version=\"1.0\" standalone=\"no\" ?>\n" port)
  (node:write self port)
  (newline port)))

; see: https://www.w3.org/TR/DOM-Level-3-Core/core.html#ID-A6C9094
; Returns a list of all the xml:element's in document order with a given tag
; name and are contained in the document.
;
; Parameters
; self: of type xml:document
;   The document in which the action is performed
; tagname: of type basic-value
;   The name of the tag to match on. The tagname parameter is case-sensitive
;
; Return Value
; A new list containing all the matched xml:element's.
(define document:getElementsByTagName (lambda (self tag-name)
   (assert (xml:document? self)
          (basic-value? tag-name)
          report: '@document:getElementsByTagName)
  (element:getElementsByTagName (xml:documentElement self) tag-name)))

; see: https://www.w3.org/TR/DOM-Level-3-Core/core.html#ID-1084891198
; Creates an xml:attribute of the given name. Note that the Attr instance
; can then be set on an xml:element using the xml:setAttributeNode method.
; Parameters
; self: of type xml:document
;   The document for which the node is created
; name: of type string
;   The name of the attribute.

;  Return Value
;   A new xml:attribute object with the nodeName attribute set to name, and
;   localName, prefix, and namespaceURI set to null. The value of the
;   attribute is the empty string.
(define xml:createAttribute (lambda (self name)
   (assert (xml:document? self)
          (string? name)
          report: '@xml:createAttribute)
  (make-attribute name self)))

;see: https://www.w3.org/TR/DOM-Level-3-Core/core.html#ID-D26C0AF8
; Creates a xml:cdata section node whose value is the specified string.

; Parameters
; self: of type xml:document
;   The document for which the node is created
; data: of type string
;   The data for the CDATASection contents.

; Return Value
;   The new xml:cdata object.
(define xml:createCDATASection (lambda (self data)
   (assert (xml:document? self)
          (string? data)
          report: '@xml:createCDATASection)
  (make-cdata data self)))


; see https://www.w3.org/TR/DOM-Level-3-Core/core.html#ID-1334481328
; Creates a xml:comment node given the specified string.
; Parameters
; self: of type xml:document
;   The document for which the node is created
; data: of type DOMString
;   The data for the node.
; Return Value
;   The new xml:comment object.
(define xml:createComment (lambda (self data)
   (assert (xml:document? self)
          (string? data)
          report: '@xml:createComment)
  (make-comment data self)))

;; Not (yet) implemented
; see: https://www.w3.org/TR/DOM-Level-3-Core/core.html#ID-35CB04B5
(define xml:createDocumentFragment (lambda (self)
   (assert (xml:document? self)
          report: '@xml:createDocumentFragment)
  (raise-dom-exception 'NOT_SUPPORTED_ERR)))

; see: https://www.w3.org/TR/DOM-Level-3-Core/core.html#ID-1975348127
; Creates a xml:text node given the specified string.
; Parameters
; self: of type xml:document
;   The document for which the node is created
; data: of type basic-value
;   The data for the node.
;
; Return Value
;   The new xml:text object.
(define xml:createTextNode (lambda (self value)
   (assert (xml:document? self)
          (basic-value? value)
          report: '@xml:createTextNode)
  (make-text value self)))

; see: https://www.w3.org/TR/DOM-Level-3-Core/core.html#ID-2141741547
; Creates an element of the type specified. Note that the instance returned
; implements the Element interface, so attributes can be specified directly on
; the returned object.
; In addition, if there are known attributes with default values, Attr nodes
; representing them are automatically created and attached to the element.

; Parameters
; self: of type xml:document
;   The document for which the node is created
; tagName: of type basic-value
;   The name of the xml:element type to instantiate. For XML, this is
;   case-sensitive, otherwise it depends on the case-sensitivity of the markup
;   language in use. In that case, the name is mapped to the canonical form of
;   that markup by the DOM implementation.

; Return Value
;   xml:element

;   A new xml:element with the nodeName attribute set to tagName, and
;   localName, prefix, and namespaceURI set to null.
(define xml:createElement (lambda (self tag-name)
   (assert (xml:document? self)
          (basic-value? tag-name)
          report: '@xml:createElement)
  (make-element tag-name self)))

;; Not (yet) implemented
; see: https://www.w3.org/TR/DOM-Level-3-Core/core.html#ID-392B75AE
(define xml:createEntityReference (lambda (self name)
   (assert (xml:document? self)
          (string? name)
          report: '@xml:createEntityReference)
  (raise-dom-exception 'NOT_SUPPORTED_ERR)))

;; Not (yet) implemented
; see: https://www.w3.org/TR/DOM-Level-3-Core/core.html#ID-135944439
(define xml:createProcessingInstruction (lambda (self target data)
   (assert (xml:document? self)
          (string? target)
          (string? data)
          report: '@xml:createProcessingInstruction)
  (raise-dom-exception 'NOT_SUPPORTED_ERR)))

; Not a part of DOM!!
; Poor-mans formatting of given document.
; formats by inserting whitespace nodes (tabs and newline) in elements
(define xml:formatDocument
 (lambda (self)
   (assert (xml:document? self)
          report: '@xml:formatDocument)
  (element:format (xml:documentElement self) 1)))

;; Not (yet) implemented
; see:
; https://www.w3.org/TR/DOM-Level-3-Core/core.html#ID-getElBId

(define xml:getElementById (lambda (self id)
  (assert (xml:document? self)
          (basic-value? id)
          report: '@xml:getElementById)
  (raise-dom-exception 'NOT_SUPPORTED_ERR)))

;; Not (yet) implemented
; see
; https://www.w3.org/TR/DOM-Level-3-Core/core.html#Core-Document-importNode
(define xml:importNode (lambda (self node deep)
  (assert (xml:document? self)
          (xml:node? node)
          (boolean? deep)
          report: '@xml:importNode)
  (raise-dom-exception 'NOT_SUPPORTED_ERR)))

; see:
; https://www.w3.org/TR/DOM-Level-3-Core/core.html#Level-2-Core-DOM-createDocType

; Creates an empty xml:document-type node. Entity declarations and notations
; are not made available. Entity reference expansions and default attribute
; additions do not occur. It is expected that a future version of the DOM will
; provide a way for populating a xml:document-type.

; Parameters
; qualifiedName: of type basic-value
;   The qualified name of the document type to be created.
; publicId: of type string
;   The external subset public basic-value.
; systemId: of type string
;   The external subset system basic-value.
; Return Value
;   xml:document-type

; A new xml:document-type node with Node.ownerDocument set to null.
(define xml:createDocumentType (lambda (name public-id system-id)
  (assert (basic-value? name)
          (string? public-id)
          (string? system-id)
          report: '@xml:createDocumentType)
  (make-document-type name public-id system-id)))

; see:
; https://www.w3.org/TR/DOM-Level-3-Core/core.html#Level-2-Core-DOM-createDocument

; Creates an XML Document object of the specified type with its document
; element.
;
; Parameters
; namespaceURI: of type string
;   The namespace URI of the document element to create. Currently not
;   supported, and should be null
; qualifiedName: of type basic-value
;   The qualified name of the document element to be created.
;   because namespace is not supported (yet) there should be no prefix
; doctype: of type xml:document-type
;   The type of document to be created or null.
;   When doctype is not null, its Node.ownerDocument attribute is set to the
;   document being created.
; Return Value
;   xml:document
; A new xml:document object.
(define xml:createDocument (lambda (namespace name doctype)
  (assert (basic-value? name)
          (or (null? doctype)(xml:document-type? doctype))
          report: '@xml:createDocument)
  (if (not (null? namespace))
    (raise-dom-exception-msg 'NOT_SUPPORTED_ERR
"\nnamespace support not (yet) implemented
first argument of createDocument should be null")
    (let* ((self (make <xml:document>
            #:symbol 'DOCUMENT_NODE
            #:document-type doctype))
            (root (xml:createElement self name)))
      (if (null? doctype) '()(xml:appendChild self doctype))
      (document:set-element! self root)
      (xml:appendChild self root)
      self))))
