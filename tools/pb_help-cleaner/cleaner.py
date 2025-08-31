import os, pathlib, shutil
from typing import cast

os.chdir(pathlib.Path(__file__).parent)

import google.protobuf.json_format as proto_json
import help


if not os.path.exists("generated"):
    gen = os.path.abspath("generated")
    os.mkdir(gen)

    os.chdir(R"../../builtins/proto")
    import grpc_tools.protoc as protoc
    code = protoc.main(
        [
            "--proto_path=.",
            "--python_out=" + gen,
            "--mypy_out=" + gen,
            "builtins.proto",
        ]
    )
    if code != 0:
        shutil.rmtree("generated")

os.chdir(pathlib.Path(__file__).parent)

import generated.builtins_pb2 as proto

funcs = proto.Functions()
with open("../../builtins/functions.json", "r") as f:
    proto_json.Parse(f.read(), funcs)

func_replacements = {
    "commandparm_func": "commandparm",
    "fileopenfunc": "fileopen_func",
    " getfileopenname_func": "getfileopenname_func",
    "ketdown_func": "keydown_func",
    "anyhebrew_func": "isanyhebrew_func",
    "anyarabic_func": "isanyarabic_func",
    "clipboard_func_syn1": "xref_40987_syntax_1",
    "clipboard_func_syn2": "xref_88082_syntax_2",
    "setremote_func_syn1": "xref_89195_syntax_1",
    "setremote_func_syn2": "xref_13099_syntax_2",
    "setpointer_func_syn1": "xref_32945_syntax_1",
    "setpointer_func_syn2": "xref_89949_syntax_2",
    "setcultureformat_func": "bk03pt03ch15s164",
    "selecttreenode": "bk03pt03ch15s156",
    "lastposa_func": "lastposa_lastposw",
    "lastposw_func": "lastposa_lastposw",
}

for func in funcs.function:
    if not func.help:
        func.help = func.name + '_func'

    if func.help in func_replacements:
        func.help = func_replacements[func.help]

    if not help.item_exists(func.help):
        func.ClearField('help')

n = 0
for func in funcs.function:
    if not func.help: continue

    n += 1
    func.help = help.get_help(func.help)

print(f'[INFO] Got help for {n} functions')
with open("../../builtins/functions.pb", "wb") as f:
    f.write(funcs.SerializeToString())

method_replacements = {
#     ("oletxnobject", "setcomplete"): "xref_34154_syntax_1",
#     ("oletxnobject", "setabort"): "xref_94225_syntax_1",
}

class_replacement = {
    'olecustomcontrol': 'OLECustomControl_control_OCX',
    'oleruntimeerror': 'RuntimeError_object',
    'dwruntimeerror': 'RuntimeError_object',
    'pbxruntimeerror': 'RuntimeError_object',
    'nullobjecterror': 'RuntimeError_object',
    'dividebyzeroerror': 'RuntimeError_object',
}

undocumented = 'ClassDefinitionObject ConnectObject CPlusPlus DragObject DrawObject DWObject ExtObject Function_Object GraphicObject NonVisualObject OmControl OmCustomControl OmEmbeddedControl OmObject OmStorage OmStream ORB PBtoCPPObject PDFObject PowerObject RemoteObject Service Structure WindowObject'.lower().split()

classes = proto.Classes()
with open("../../builtins/classes.json", "r") as f:
    proto_json.Parse(f.read(), classes)

for cls in cast(list[proto.Class], getattr(classes, "class")):
    if cls.name in class_replacement:
        cls.help = help.get_help(class_replacement[cls.name])
    else:
        for pot in [cls.help, cls.name+'_object', cls.name+'_control']:
            if help.item_exists(pot):
                cls.help = help.get_help(pot)
                break
        else:
            # if not cls.name.lower() in undocumented:
            #     print(cls.name, cls.help)
            cls.ClearField('help')

    for func in cls.function:
        if not func.help:
            func.help = func.name

        if (cls.name, func.name) in method_replacements:
            func.help = method_replacements[(cls.name, func.name)]
            continue

        possible = [func.help]

        if cls.name in ["datawindow", "datastore", "datawindowchild"]:
            possible.append("dwmeth_" + func.help)

        for pos in possible:
            if pos.startswith(cls.name + "_"):
                possible.append(pos.removeprefix(cls.name + "_"))

        for pos in possible:
            if not pos.endswith("_func"):
                possible.append(pos + "_func")

        for pos in possible:
            if pos.endswith("_func"):
                new = pos.removesuffix("_func")
                if new not in possible:
                    possible.append(new)

        for pos in possible:
            if pos in func_replacements:
                possible.append(func_replacements[pos])

        if func.help.removeprefix(cls.name + "_").removesuffix("_func") != func.name:
            possible.extend(
                [
                    cls.name + "_" + func.name + "_func",
                    cls.name + "_" + func.name,
                    func.name + "_func",
                    func.name,
                ]
            )

        for pos in possible:
            if help.item_exists(pos):
                func.help = pos
                break
        else:
            func.ClearField('help')
    

    n = 0
    for func in cls.function:
        if not func.help: continue

        n += 1
        func.help = help.get_help(func.help)

    if n > 0:
        print(f'[INFO] Got help for {n} methods of {cls.name}')

with open("../../builtins/classes.pb", "wb") as f:
    f.write(classes.SerializeToString())
    

enums = proto.Enums()
with open("../../builtins/enums.json", "r") as f:
    proto_json.Parse(f.read(), enums)

with open("../../builtins/enums.pb", "wb") as f:
    f.write(enums.SerializeToString())
