port module Store exposing (putUS, putTask)

import Json.Encode as Encode
import US as US


port storePort : Encode.Value -> Cmd msg


type StoreItem = StoreUS US.US | StoreTask US.Task


putUS : US.US -> Cmd msg
putUS us = put <| StoreUS us


putTask : US.Task -> Cmd msg
putTask task = put <| StoreTask task


put : StoreItem -> Cmd msg
put item = storePort <| encodeItem item


encodeUS : US.US -> Encode.Value
encodeUS us =
    Encode.object [
        ("id", Encode.int us.id),
        ("name", Encode.string us.name),
        ("description", Encode.string us.description)
        ]


encodeTask : US.Task -> Encode.Value
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
