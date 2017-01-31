module Second =
  let hey = "hi"
module First =
  let greet = Second.hey
let greeting = First.greet