module Text.Slice
  ( Text(..)
  )

data Text = Text
  !TextArray -- payload
  !Int -- offset in bytes, not in characters
  !Int -- length in bytes, not in characters
