; extends
(call_expression
  (member_expression) @f (#eq? @f "prisma.$queryRaw")
  ((template_string) @injection.content
    (#set! injection.language "sql")
    (#offset! @injection.content 0 1 0 0)
    )
  )
