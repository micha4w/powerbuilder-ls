#include <Poco/UTF16Encoding.h>
#include <Poco/UTF8Encoding.h>
#include <Poco/UnicodeConverter.h>
#include <google/protobuf/util/json_util.h>
#include <ocidl.h>

#include <fstream>
#include <iostream>
#include <map>
#include <optional>
#include <unordered_set>
#include <vector>

#include "builtins.pb.h"

std::string convertUTF16(const wchar_t* utf16)
{
    std::string utf8;
    Poco::UnicodeConverter().convert(utf16, utf8);
    std::transform(utf8.begin(), utf8.end(), utf8.begin(), [](unsigned char c) { return std::tolower(c); });
    return utf8;
}

void saveToFile(const std::filesystem::path& path, const google::protobuf::Message& msg)
{
    std::string out;

    auto status = google::protobuf::util::MessageToJsonString(
        msg,
        &out,
        google::protobuf::json::PrintOptions{
            .add_whitespace = true,
        }
    );
    if (!status.ok())
        throw std::runtime_error(std::string(status.message()));

    // Use json because its easier to work with, and during processing we dont need to be efficient
    std::ofstream(path.string() + ".json") << out;

    // msg.SerializeToString(&out);
    // std::ofstream(path.string() + ".pb") << out;
}

std::optional<Function> retrieveFunction(POB_THIS obThis, OB_CLASS_HNDL cls, OB_VTABLE_ID mid)
{
    UINT no_args;
    LPTSTR name, type, dll;
    OB_MEMBER_ACCESS access;
    BOOL is_obsolete, is_inherit;
    OB_EVT_TOKEN_ID evt_id;  // TODO check this
    // ob_get_event_id_from_token();
    OB_ROUT_TYPE rout_type;
    POB_ARG_INFO args = ob_get_proto_info(
        obThis, cls, mid, &no_args, &name, &type, &access, &dll, &is_obsolete, &evt_id, &rout_type, &is_inherit
    );

    if (is_inherit || access != OB_PUBLIC_MEMBER)
        return {};

    Function func;
    func.set_name(convertUTF16(name));
    if (type && type[0] != 0)
        func.set_ret(convertUTF16(type));

    for (UINT i = 0; i < no_args; i++)
    {
        Variable* var = func.add_argument();
        if (args[i].argtype == OB_ARG_VARLIST)
        {
            var->set_flags(Variable_Flag_IS_VARLIST);
        }
        else
        {
            var->set_flags(
                (args[i].argtype == OB_ARG_REF ? Variable_Flag_IS_REF : 0) |
                (args[i].argtype == OB_ARG_READONLY ? Variable_Flag_NO_WRITE : 0)
            );
            var->set_name(convertUTF16(args[i].argname));
            var->set_type(convertUTF16(args[i].datatype));
            if (args[i].array_bounds)
                var->set_arraybounds(convertUTF16(args[i].array_bounds));
        }
    }

    POB_CLASS_ID throws_list;
    UINT no_throws;
    OB_GROUP_ID group_id;
    ob_get_proto_throws_info(obThis, cls, mid, &throws_list, &no_throws, &group_id);

    for (UINT i = 0; i < no_throws; i++)
    {
        UINT no;
        OB_GROUP_HNDL grp = ob_get_system_group_hndl(obThis);
        OB_CLASS_HNDL hndl = ob_global_reference_of_class(obThis, NULL, &grp, throws_list[i]);

        *func.add_throws() = convertUTF16(ob_class_name(obThis, hndl));
    }

    LPTSTR help = ob_get_routine_property(obThis, cls, mid, std::wstring(L"pb_help").data());
    if (help)
        func.set_help(convertUTF16(help));

    return func;
}

void exportStuffToDirectory(POB_THIS obThis, std::filesystem::path outDir)
{
    std::filesystem::create_directories(outDir);

    UINT count = 0;
    POB_CLASS_INFO classes = ob_get_classes_within_group(obThis, ob_get_system_group_hndl(obThis), 16508, &count);

    Classes out_classes;
    for (UINT i = 0; i < count; i++)
    {
        Class& current = *out_classes.add_class_();
        OB_CLASS_HNDL parent = ob_get_parent_class(obThis, classes[i].class_hndl);

        LPTSTR name = ob_class_name(obThis, classes[i].class_hndl);
        current.set_name(convertUTF16(name));
        current.set_base(convertUTF16(ob_class_name(obThis, parent)));
        LPTSTR help = ob_get_class_property(obThis, classes[i].class_hndl, std::wstring(L"pb_help").data());
        if (help)
            current.set_help(convertUTF16(help));

        UINT no_items;
        POB_VTABLE_ID mids = ob_get_function_vtable_ids(obThis, classes[i].class_hndl, TRUE, FALSE, &no_items);
        for (UINT j = 0; j < no_items; j++)
        {
            std::optional<Function> func = retrieveFunction(obThis, classes[i].class_hndl, mids[j]);
            if (func.has_value())
                *current.add_function() = std::move(func.value());
        }

        POB_VTABLE_ID eids = ob_get_event_vtable_ids(obThis, classes[i].class_hndl, TRUE, &no_items);
        for (UINT j = 0; j < no_items; j++)
        {
            std::optional<Function> func = retrieveFunction(obThis, classes[i].class_hndl, eids[j]);
            if (func.has_value())
                *current.add_event() = std::move(func.value());
        }

        std::unordered_set<std::wstring> inherited_fields;
        POB_DATA_INFO base_fields = ob_get_class_field_info(obThis, parent, &no_items);
        for (UINT j = 0; j < no_items; j++)
        {
            if (base_fields && base_fields[j].scope == OB_PUBLIC_MEMBER)
                inherited_fields.insert(base_fields[j].name);
        }

        POB_DATA_INFO fields = ob_get_class_field_info(obThis, classes[i].class_hndl, &no_items);
        for (UINT j = 0; j < no_items; j++)
        {
            if (inherited_fields.contains(fields[j].name) || fields[j].scope != OB_PUBLIC_MEMBER)
                continue;

            Variable& var = *current.add_variable();
            var.set_name(convertUTF16(fields[j].name));
            var.set_type(convertUTF16(ob_class_name(obThis, fields[j].class_hndl)));
            var.set_flags(0);
            LPTSTR help =
                ob_get_field_property(obThis, classes[i].class_hndl, fields[j].sym_id, std::wstring(L"pb_help").data());
            if (help)
                var.set_help(convertUTF16(help));

            wchar_t* bounds = ob_get_array_bounds_string_from_field_info(obThis, &fields[j]);
            if (bounds)
            {
                var.set_arraybounds(convertUTF16(bounds));
                var.set_flags(var.flags() | Variable_Flag_IS_ARRAY);
            }

            if (fields[j].read_access != OB_PUBLIC_MEMBER)
                var.set_flags(var.flags() | Variable_Flag_NO_READ);

            if (fields[j].write_access != OB_PUBLIC_MEMBER)
                var.set_flags(var.flags() | Variable_Flag_NO_WRITE);

            if (fields[j].flags & DATA_INFO_INDIRECT_MASK)
            {
                if (!(fields[j].flags & DATA_INFO_ISREAD_MASK))
                    var.set_flags(var.flags() | Variable_Flag_NO_READ);

                if (!(fields[j].flags & DATA_INFO_ISWRITE_MASK))
                    var.set_flags(var.flags() | Variable_Flag_NO_WRITE);
            }

            if (fields[j].flags & DATA_INFO_CONSTANT_MASK)
                var.set_flags(var.flags() | Variable_Flag_NO_WRITE);
        }
    }

    saveToFile(outDir / "classes", out_classes);

    OB_CLASS_ID cid = ob_get_system_func_class(obThis)->GetClassID();
    OB_GROUP_HNDL grp = ob_get_system_group_hndl(obThis);
    OB_CLASS_HNDL hndl = ob_global_reference_of_class(obThis, std::wstring(L"systemfunctions").data(), &grp, cid);

    Functions functions;
    POB_VTABLE_ID mids = ob_get_function_vtable_ids(obThis, hndl, TRUE, FALSE, &count);
    for (UINT j = 0; j < count; j++)
    {
        std::optional<Function> func = retrieveFunction(obThis, hndl, mids[j]);
        if (func.has_value())
            *functions.add_function() = std::move(func.value());
    }

    saveToFile(outDir / "functions", functions);

    Enums out_enums;
    POB_CLASS_INFO enums = ob_get_enums_within_group(obThis, NULL, &count);
    for (UINT i = 0; i < count; i++)
    {
        UINT no_enums;
        POB_ENUM_INFO info = ob_get_enum_info(obThis, enums[i].class_hndl, &no_enums);

        std::vector<std::string> names;
        for (UINT j = 0; j < no_enums; j++)
        {
            names.push_back(convertUTF16(info[j].name));
        }

        LPTSTR help = ob_get_class_property(obThis, enums[i].class_hndl, std::wstring(L"pb_help").data());
        Enum& enum_ = *out_enums.add_enum_();
        enum_.set_name(convertUTF16(enums[i].classname));
        if (help)
            enum_.set_help(convertUTF16(help));

        for (std::string& name : names)
            *enum_.add_value() = std::move(name);
    }

    saveToFile(outDir / "enums", out_enums);
}