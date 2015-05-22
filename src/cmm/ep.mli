type ('em, 'pr) pre_map =
    { 
      embed   : 'em; 
      project : 'pr
    }
      
type ('lo, 'hi) map = ('lo -> 'hi, 'hi -> 'lo) pre_map
