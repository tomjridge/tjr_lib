let counter = ref 0

let gensym () = (
  let r = !counter in
  counter:=r+1; r)
