module Query.Var exposing (beforeVar, firstVar, lastVar, nameVar, ownerVar, qualifiedNameVar)

import GraphQL.Request.Builder.Variable as Var


ownerVar : Var.Variable { vars | owner : String }
ownerVar =
    Var.required "owner" .owner Var.string


nameVar : Var.Variable { vars | name : String }
nameVar =
    Var.required "name" .name Var.string


qualifiedNameVar : Var.Variable { vars | qualifiedName : String }
qualifiedNameVar =
    Var.required "qualifiedName" .qualifiedName Var.string


firstVar : Var.Variable { vars | first : Int }
firstVar =
    Var.required "first" .first Var.int


beforeVar : Var.Variable { vars | before : String }
beforeVar =
    Var.required "before" .before Var.string


lastVar : Var.Variable { vars | last : Int }
lastVar =
    Var.required "last" .last Var.int
