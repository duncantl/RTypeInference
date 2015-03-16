setClass("BasicTypeInfo", representation(name = "character"), contains = "character")
           

setClass("UpdateType", contains = "BasicTypeInfo")
#          representation(name = "character"))
UpdateType =
function(type, name, obj = new("UpdateType"))
{
#  obj@type = as.character(type)
  obj@.Data = as.character(type)
  obj@name = as.character(name)
  obj
}

