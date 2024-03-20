-- Rerun tests only if their modification time changed.
allow_defined = true
cache = true
files['.luacheckrc'].ignore = { '111', '112', '131' }

ignore = {
  '111',
  '112',
  '131',
  '631', -- max_line_length
  '212/_.*', -- unused argument, for vars with "_" prefix
  '214', -- used variable with unused hint ("_" prefix)
  '121', -- setting read-only global variable 'vim'
  '122', -- setting read-only field of global variable 'vim'
  '581', -- negation of a relational operator- operator can be flipped (not for tables)
}

-- Global objects defined by the C code
globlas = {
  'vim',
}
read_globals = {
  'vim',
}
