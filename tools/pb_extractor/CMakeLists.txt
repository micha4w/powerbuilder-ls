cmake_minimum_required(VERSION 3.27)

project(pb_extractor VERSION 0.0.1 LANGUAGES CXX C)


add_executable(${PROJECT_NAME}
	src/main.cpp
)

target_sources(${PROJECT_NAME} PRIVATE
	src/export.cpp
	src/export.h
)

target_compile_features(${PROJECT_NAME} PUBLIC cxx_std_20)
target_compile_definitions(${PROJECT_NAME}
PUBLIC
	WIN32_LEAN_AND_MEAN
	UNICODE
	_UNICODE
)

find_package(Poco REQUIRED Foundation)
find_package(Protobuf CONFIG REQUIRED)

add_library(proto-objects OBJECT "${CMAKE_CURRENT_LIST_DIR}/../../proto/builtins.proto")
target_link_libraries(proto-objects PUBLIC protobuf::libprotobuf)

set(PROTO_BINARY_DIR "${CMAKE_CURRENT_BINARY_DIR}/generated")
target_include_directories(proto-objects PUBLIC "$<BUILD_INTERFACE:${PROTO_BINARY_DIR}>")

protobuf_generate(
    TARGET proto-objects
    IMPORT_DIRS "${CMAKE_CURRENT_LIST_DIR}/../../proto"
    PROTOC_OUT_DIR "${PROTO_BINARY_DIR}")


set(PBVM_LIB "${CMAKE_CURRENT_BINARY_DIR}/pbvm.lib")
set(PBVM_DEF "${CMAKE_SOURCE_DIR}/pb/lib/pbvm.def")
set(PBSHR_LIB "${CMAKE_CURRENT_BINARY_DIR}/pbshr.lib")
set(PBSHR_DEF "${CMAKE_SOURCE_DIR}/pb/lib/pbshr.def")

add_custom_command(
	OUTPUT "${PBVM_LIB}"
	COMMAND lib.exe /machine:x86 /def:"${PBVM_DEF}" /out:"${PBVM_LIB}"
	DEPENDS "${PBVM_DEF}"
)
add_custom_command(
	OUTPUT "${PBSHR_LIB}"
	COMMAND lib.exe /machine:x86 /def:"${PBSHR_DEF}" /out:"${PBSHR_LIB}"
	DEPENDS "${PBSHR_DEF}"
)
add_custom_target(libs ALL DEPENDS "${PBVM_LIB}" "${PBSHR_LIB}")

target_link_libraries(${PROJECT_NAME}
PRIVATE
	Poco::Foundation
	protobuf::libprotoc
	proto-objects
	${PBVM_LIB}
	${PBSHR_LIB}
)

target_precompile_headers(${PROJECT_NAME}
PRIVATE
	"$<$<COMPILE_LANGUAGE:CXX>:<windows.h>>" 
	"$<$<COMPILE_LANGUAGE:CXX>:<Shlwapi.h>>" 
	"$<$<COMPILE_LANGUAGE:CXX>:<iostream>>" 
	"$<$<COMPILE_LANGUAGE:CXX>:${CMAKE_SOURCE_DIR}/pb/include/pb_types.h>" 
)