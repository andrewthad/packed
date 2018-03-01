module Text.Slice
  ( Text(..)
  )

data Text = Text
  !TextArray -- payload
  !Word -- offset in bytes, not in characters, first bit reserved
  !Int -- length in bytes, not in characters
