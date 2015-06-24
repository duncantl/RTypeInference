# Description:
#   S4 classes for types.

setClass("Type")

# Scalar Types --------------------------------------------------
setClass("ScalarType", contains = "Type")

setClass("LogicalType", contains = "ScalarType")
setClass("IntegerType", contains = "ScalarType")
setClass("NumericType", contains = "ScalarType")
setClass("ComplexType", contains = "ScalarType")
setClass("CharacterType", contains = "ScalarType")
setClass("ListType", contains = "ScalarType")

# Vector Types --------------------------------------------------
setClass("VectorType", contains = "Type",
  slots = list(
    length = "integer"
  )
) 

setClass("LogicalVectorType", contains = "VectorType")
setClass("IntegerVectorType", contains = "VectorType")
setClass("NumericVectorType", contains = "VectorType")
setClass("ComplexVectorType", contains = "VectorType")
setClass("CharacterVectorType", contains = "VectorType")
setClass("ListVectorType", contains = "VectorType")

