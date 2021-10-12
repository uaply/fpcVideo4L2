unit uvcvideo;

interface

{
  Automatically converted by H2Pas 1.0.0 from uvcvideo.h
  The following command line parameters were used:
    -e
    -p
    uvcvideo.h
}

    { Pointers to basic pascal types, inserted by h2pas conversion program.}
{    Type
      PLongint  = ^Longint;
      PSmallInt = ^SmallInt;
      PByte     = ^Byte;
      PWord     = ^Word;
      PDWord    = ^DWord;
      PDouble   = ^Double;
}{
    Type
    P__u8  = ^__u8;
    Puvc_menu_info  = ^uvc_menu_info;
    Puvc_xu_control_mapping  = ^uvc_xu_control_mapping;
    Puvc_xu_control_query  = ^uvc_xu_control_query;
}
{$IFDEF FPC}
{$PACKRECORDS C}
{$ENDIF}


{$ifndef __LINUX_UVCVIDEO_H_}
{$define __LINUX_UVCVIDEO_H_}  
{//#include <linux/ioctl.h>}
  const
     _IOC_NRBITS = 8;
     _IOC_TYPEBITS = 8;
     _IOC_SIZEBITS = 14;
     _IOC_DIRBITS = 2;
     _IOC_NRMASK = (1 shl _IOC_NRBITS)-1;
     _IOC_TYPEMASK = (1 shl _IOC_TYPEBITS)-1;
     _IOC_SIZEMASK = (1 shl _IOC_SIZEBITS)-1;
     _IOC_DIRMASK = (1 shl _IOC_DIRBITS)-1;
     _IOC_NRSHIFT = 0;
     _IOC_TYPESHIFT = _IOC_NRSHIFT+_IOC_NRBITS;
     _IOC_SIZESHIFT = _IOC_TYPESHIFT+_IOC_TYPEBITS;
     _IOC_DIRSHIFT = _IOC_SIZESHIFT+_IOC_SIZEBITS;
  {
   * Direction bits.
    }
     _IOC_NONE = 0;
     _IOC_WRITE = 1;
     _IOC_READ = 2;
{//#include <linux/types.h>}
type
  __u8 = byte;
  __u16 = word;
  __u32 = LongWord;
  
  {
   * Dynamic controls
    }
  { Data types for UVC control data  }

  const
    UVC_CTRL_DATA_TYPE_RAW = 0;    
    UVC_CTRL_DATA_TYPE_SIGNED = 1;    
    UVC_CTRL_DATA_TYPE_UNSIGNED = 2;    
    UVC_CTRL_DATA_TYPE_BOOLEAN = 3;    
    UVC_CTRL_DATA_TYPE_ENUM = 4;    
    UVC_CTRL_DATA_TYPE_BITMASK = 5;    
  { Control flags  }
    UVC_CTRL_FLAG_SET_CUR = 1 shl 0;    
    UVC_CTRL_FLAG_GET_CUR = 1 shl 1;    
    UVC_CTRL_FLAG_GET_MIN = 1 shl 2;    
    UVC_CTRL_FLAG_GET_MAX = 1 shl 3;    
    UVC_CTRL_FLAG_GET_RES = 1 shl 4;    
    UVC_CTRL_FLAG_GET_DEF = 1 shl 5;    
  { Control should be saved at suspend and restored at resume.  }
    UVC_CTRL_FLAG_RESTORE = 1 shl 6;    
  { Control can be updated by the camera.  }
    UVC_CTRL_FLAG_AUTO_UPDATE = 1 shl 7;    
    UVC_CTRL_FLAG_GET_RANGE = (((UVC_CTRL_FLAG_GET_CUR or UVC_CTRL_FLAG_GET_MIN) or UVC_CTRL_FLAG_GET_MAX) or UVC_CTRL_FLAG_GET_RES) or UVC_CTRL_FLAG_GET_DEF;    

  type
    Puvc_menu_info = ^uvc_menu_info;
    uvc_menu_info = record
        value : __u32;
        name : array[0..31] of __u8;
      end;

  {__user }    Puvc_xu_control_mapping = ^uvc_xu_control_mapping;
    uvc_xu_control_mapping = record
        id : __u32;
        name : array[0..31] of __u8;
        entity : array[0..15] of __u8;
        selector : __u8;
        size : __u8;
        offset : __u8;
        v4l2_type : __u32;
        data_type : __u32;
        menu_info : Puvc_menu_info;
        menu_count : __u32;
        reserved : array[0..3] of __u32;
      end;

  { Video Class-Specific Request Code,  }
  { defined in linux/usb/video.h A.8.   }
  {__user }    Puvc_xu_control_query = ^uvc_xu_control_query;
    uvc_xu_control_query = record
        _unit : __u8;
        selector : __u8;
        query : __u8;
        size : __u16;
        data : ^__u8;
      end;

const
  UVCIOC_CTRL_MAP         = LongInt(((_IOC_READ or _IOC_WRITE) shl _IOC_DIRSHIFT) or (Ord('u') shl _IOC_TYPESHIFT) or ($20 shl _IOC_NRSHIFT) or (SizeOf(uvc_xu_control_mapping) shl _IOC_SIZESHIFT));
  UVCIOC_CTRL_QUERY       = LongInt(((_IOC_READ or _IOC_WRITE) shl _IOC_DIRSHIFT) or (Ord('u') shl _IOC_TYPESHIFT) or ($21 shl _IOC_NRSHIFT) or (SizeOf(uvc_xu_control_query) shl _IOC_SIZESHIFT));

{$endif}

implementation

end.
