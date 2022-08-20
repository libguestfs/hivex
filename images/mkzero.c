/* use the NT native API to create registry key and value that contain
   a zero character */

#include <ntdef.h>
#include <stdio.h>
#include <ddk/wdm.h>
#include <windef.h>

void create_key_value (PHANDLE handle, WCHAR* key, int key_len, WCHAR* val, int val_len)
{
  UNICODE_STRING key_name = { key_len, key_len, key };
  UNICODE_STRING value_name = { val_len, val_len, val };
  OBJECT_ATTRIBUTES key_obj;
  InitializeObjectAttributes (&key_obj, &key_name,
                              OBJ_OPENIF | OBJ_CASE_INSENSITIVE,
                              *handle, NULL);
  HANDLE key_handle;
  NTSTATUS rc;
  rc = ZwCreateKey (&key_handle, KEY_ALL_ACCESS, &key_obj,
                    0, NULL, REG_OPTION_NON_VOLATILE, NULL);
  if (!NT_SUCCESS (rc)) {
    wprintf(L"error: CreateKey %s: 0x%08x\n", key, rc);
    exit(1);
  }
  DWORD value = 0;
  rc = ZwSetValueKey (key_handle, &value_name, 0,
                      REG_DWORD, &value, sizeof(value));
  if (!NT_SUCCESS (rc)) {
    wprintf(L"error: SetValueKey %s: 0x%08x\n", val, rc);
    exit(1);
  }
}

int main (int argc, char **argv)
{
  UNICODE_STRING root_key_name;
  RtlInitUnicodeString(&root_key_name, L"\\Registry\\Machine\\minimal");
  OBJECT_ATTRIBUTES root_key_obj;
  InitializeObjectAttributes (&root_key_obj, &root_key_name,
                              OBJ_OPENIF | OBJ_CASE_INSENSITIVE,
                              NULL, NULL);
  HANDLE minimal_key_handle;
  NTSTATUS rc = ZwCreateKey (&minimal_key_handle, KEY_ALL_ACCESS, &root_key_obj,
                    0, NULL, REG_OPTION_NON_VOLATILE, NULL);
  if (!NT_SUCCESS (rc)) {
    wprintf(L"error: CreateKey <HKLM\\minimal>: 0x%08x\n", rc);
    exit(1);
  }
  WCHAR k1[] = L"zero\0key";
  WCHAR v1[] = L"zero\0val";
  create_key_value (&minimal_key_handle, k1, sizeof (k1)-2, v1, sizeof (v1)-2);
  WCHAR k2[] = L"abcd_äöüß";
  WCHAR v2[] = L"abcd_äöüß";
  create_key_value (&minimal_key_handle, k2, sizeof (k2)-2, v2, sizeof (v2)-2);
  WCHAR k3[] = L"weird™";
  WCHAR v3[] = L"symbols $£₤₧€";
  create_key_value (&minimal_key_handle, k3, sizeof (k3)-2, v3, sizeof (v3)-2);
  return 0;
}
