; inject into cursor.execute("")
(call
  (attribute
    object: (identifier) @obj (#eq? @obj "cursor")
    attribute: (identifier) @att (#eq? @att "execute"))

  (argument_list
     (string) @sql
     (#offset! @sql 1 0 0 0))
  )
