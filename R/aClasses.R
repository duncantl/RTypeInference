setClass("UpdateType",
          representation(type = "character", name = "character"))
UpdateType =
function(type, name, obj = new("UpdateType"))
{
  obj@type = type
  obj@name = name
  obj
}
