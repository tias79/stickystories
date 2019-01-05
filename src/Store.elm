port module Store exposing (putUS, putTask)

import Json.Encode as Encode
import US as US
import USTask as Task


port storePort : Encode.Value -> Cmd msg


type StoreItem = StoreUS US.T | StoreTask Task.T


putUS : US.T -> Cmd msg
putUS us = put <| StoreUS us


putTask : Task.T -> Cmd msg
putTask task = put <| StoreTask task


put : StoreItem -> Cmd msg
put item = storePort <| encodeItem item


encodeUS : US.T -> Encode.Value
encodeUS us =
    Encode.object [
        ("id", Encode.int us.id),
        ("name", Encode.string us.name),
        ("description", Encode.string us.description)
        ]


encodeTask : Task.T -> Encode.Value
encodeTask task =
    Encode.object [
        ("id", Encode.int task.id),
        ("description", Encode.string task.description)
        ]


encodeItem : StoreItem -> Encode.Value
encodeItem item =
    case item of
        StoreUS us -> Encode.object [("type", Encode.string "us"), ("item", encodeUS us)]
        StoreTask task -> Encode.object [("type", Encode.string "task"), ("item", encodeTask task)]
