#include <filesystem>
#include <fstream>
#include <iostream>
#include <map>
#include <optional>
#include <unordered_set>
#include <vector>
#include <libloaderapi.h>

#include "export.h"

extern "C" PBWINAPI(POB_THIS, FN_RuntimeCreate)(
    HMODULE module,
    const char* name,
    LPTSTR* lib_list,
    UINT lib_list_count,
    ppbstg_anchor anchor,
    UINT idk1,
    UINT idk2,
    UINT* res
);

int main(int argc, char** argv)
{
    bool help = argc > 1;
    for (int i = 1; i < argc; i++)
    {
        if (argv[i] == "-h" || argv[i] == "--help")
        help = true;
    }
    if (help)
    {
        std::cout << "Has to be ran inside the same directory as pbvm.dll\n";
        std::cout << "Usage: " << argv[0] << " <directory where to drop the proto files>" << std::endl;
        return 0;
    }

    UINT res;
    ppbstg_anchor anc = pbstg_begin(0);
    auto obThis = FN_RuntimeCreate(GetModuleHandleA(NULL), NULL, NULL, 0, anc, 0, 0, &res);
    if (obThis == NULL)
    {
        std::cout << "FN_RuntimeCreate failed with code " << res << std::endl;
        return 1;
    }

    try {
        exportStuffToDirectory(obThis, argc > 1 ? argv[1] : ".");
    } catch (const std::exception& ex) {
        std::cout << "Failed to export: " << ex.what() << std::endl;
        return 1;
    }

    return 0;
}