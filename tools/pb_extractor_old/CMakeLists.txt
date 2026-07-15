cmake_minimum_required(VERSION 3.27)

project(pb_extractor VERSION 0.0.1 LANGUAGES CXX C)

set(PB_RUNTIME "C:/Program Files (x86)/Appeon/Common/PowerBuilder/Runtime 22.2.0.3356" CACHE PATH "Path to the PowerBuilder runtime")
set(OUT_DIR "${CMAKE_SOURCE_DIR}/../../system/" CACHE PATH "Path of where to throw the Proto objects at")

add_library(${PROJECT_NAME}
SHARED
	src/dll.cpp
	src/dll.def
)
target_compile_features(${PROJECT_NAME} PUBLIC cxx_std_20)
target_compile_definitions(${PROJECT_NAME}
PUBLIC
	WIN32_LEAN_AND_MEAN
	UNICODE
	_UNICODE
	"OUT_DIR=\"${OUT_DIR}\""
)

find_package(Poco REQUIRED Foundation)
find_package(Protobuf CONFIG REQUIRED)

add_library(proto-objects OBJECT "${CMAKE_CURRENT_LIST_DIR}/../../proto/output.proto")
target_link_libraries(proto-objects PUBLIC protobuf::libprotobuf)

set(PBVM_LIB "${CMAKE_CURRENT_BINARY_DIR}/pbvm.lib")
set(PBVM_DEF "${CMAKE_SOURCE_DIR}/pb/lib/pbvm.def")

set(PROTO_BINARY_DIR "${CMAKE_CURRENT_BINARY_DIR}/generated")
target_include_directories(proto-objects PUBLIC "$<BUILD_INTERFACE:${PROTO_BINARY_DIR}>")

protobuf_generate(
    TARGET proto-objects
    IMPORT_DIRS "${CMAKE_CURRENT_LIST_DIR}/../../proto"
    PROTOC_OUT_DIR "${PROTO_BINARY_DIR}")

add_custom_command(
	OUTPUT "${PBVM_LIB}"
	COMMAND lib.exe /machine:x86 /def:"${PBVM_DEF}" /out:"${PBVM_LIB}"
	DEPENDS "${PBVM_DEF}"
)
add_custom_target(pbvm-lib ALL DEPENDS "${PBVM_LIB}")

target_link_libraries(${PROJECT_NAME}
PRIVATE
	Poco::Foundation
	protobuf::libprotoc
	proto-objects
	${PBVM_LIB}
)

set_target_properties(${PROJECT_NAME}
PROPERTIES
	VS_DEBUGGER_WORKING_DIRECTORY "${PB_RUNTIME}"
)

target_precompile_headers(${PROJECT_NAME}
PRIVATE
	"$<$<COMPILE_LANGUAGE:CXX>:<windows.h>>" 
	"$<$<COMPILE_LANGUAGE:CXX>:<Shlwapi.h>>" 
	"$<$<COMPILE_LANGUAGE:CXX>:<iostream>>" 
	"$<$<COMPILE_LANGUAGE:CXX>:${CMAKE_SOURCE_DIR}/pb/include/pb_types.h>" 
)

install(TARGETS ${PROJECT_NAME}
	RUNTIME DESTINATION "${CMAKE_CURRENT_LIST_DIR}/proj"
)