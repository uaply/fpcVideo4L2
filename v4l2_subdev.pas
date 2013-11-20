unit v4l2_subdev;

interface

uses
  videodev2;

{
  Automatically converted by H2Pas 1.0.0 from v4l2-subdev.h
  The following command line parameters were used:
    -e
    -p
    v4l2-subdev.h
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
    Pv4l2_subdev_crop  = ^v4l2_subdev_crop;
    Pv4l2_subdev_edid  = ^v4l2_subdev_edid;
    Pv4l2_subdev_format  = ^v4l2_subdev_format;
    Pv4l2_subdev_format_whence  = ^v4l2_subdev_format_whence;
    Pv4l2_subdev_frame_interval  = ^v4l2_subdev_frame_interval;
    Pv4l2_subdev_frame_interval_enum  = ^v4l2_subdev_frame_interval_enum;
    Pv4l2_subdev_frame_size_enum  = ^v4l2_subdev_frame_size_enum;
    Pv4l2_subdev_mbus_code_enum  = ^v4l2_subdev_mbus_code_enum;
    Pv4l2_subdev_selection  = ^v4l2_subdev_selection;
}
{$IFDEF FPC}
{$PACKRECORDS C}
{$ENDIF}


  {
   * V4L2 subdev userspace API
   *
   * Copyright (C) 2010 Nokia Corporation
   *
   * Contacts: Laurent Pinchart <laurent.pinchart@ideasonboard.com>
   *	     Sakari Ailus <sakari.ailus@iki.fi>
   *
   * This program is free software; you can redistribute it and/or modify
   * it under the terms of the GNU General Public License version 2 as
   * published by the Free Software Foundation.
   *
   * This program is distributed in the hope that it will be useful,
   * but WITHOUT ANY WARRANTY; without even the implied warranty of
   * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   * GNU General Public License for more details.
   *
   * You should have received a copy of the GNU General Public License
   * along with this program; if not, write to the Free Software
   * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
    }
{$ifndef __LINUX_V4L2_SUBDEV_H}
{$define __LINUX_V4L2_SUBDEV_H}  
{//#include <linux/ioctl.h>}
{//#include <linux/types.h>}
{//#include <linux/v4l2-common.h>}
  {
   * include/linux/v4l2-common.h
   *
   * Common V4L2 and V4L2 subdev definitions.
   *
   * Users are advised to #include this file either through videodev2.h
   * (V4L2) or through v4l2-subdev.h (V4L2 subdev) rather than to refer
   * to this file directly.
   *
   * Copyright (C) 2012 Nokia Corporation
   * Contact: Sakari Ailus <sakari.ailus@iki.fi>
   *
   * This program is free software; you can redistribute it and/or
   * modify it under the terms of the GNU General Public License
   * version 2 as published by the Free Software Foundation.
   *
   * This program is distributed in the hope that it will be useful, but
   * WITHOUT ANY WARRANTY; without even the implied warranty of
   * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   * General Public License for more details.
   *
   * You should have received a copy of the GNU General Public License
   * along with this program; if not, write to the Free Software
   * Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA
   * 02110-1301 USA
   *
    }
{$ifndef __V4L2_COMMON__}
{$define __V4L2_COMMON__}  
  {
   *
   * Selection interface definitions
   *
    }
  { Current cropping area  }

  const
    V4L2_SEL_TGT_CROP = $0000;    
  { Default cropping area  }
    V4L2_SEL_TGT_CROP_DEFAULT = $0001;    
  { Cropping bounds  }
    V4L2_SEL_TGT_CROP_BOUNDS = $0002;    
  { Current composing area  }
    V4L2_SEL_TGT_COMPOSE = $0100;    
  { Default composing area  }
    V4L2_SEL_TGT_COMPOSE_DEFAULT = $0101;    
  { Composing bounds  }
    V4L2_SEL_TGT_COMPOSE_BOUNDS = $0102;    
  { Current composing area plus all padding pixels  }
    V4L2_SEL_TGT_COMPOSE_PADDED = $0103;    
  { Backward compatibility target definitions --- to be removed.  }
    V4L2_SEL_TGT_CROP_ACTIVE = V4L2_SEL_TGT_CROP;    
    V4L2_SEL_TGT_COMPOSE_ACTIVE = V4L2_SEL_TGT_COMPOSE;    
    V4L2_SUBDEV_SEL_TGT_CROP_ACTUAL = V4L2_SEL_TGT_CROP;    
    V4L2_SUBDEV_SEL_TGT_COMPOSE_ACTUAL = V4L2_SEL_TGT_COMPOSE;    
    V4L2_SUBDEV_SEL_TGT_CROP_BOUNDS = V4L2_SEL_TGT_CROP_BOUNDS;    
    V4L2_SUBDEV_SEL_TGT_COMPOSE_BOUNDS = V4L2_SEL_TGT_COMPOSE_BOUNDS;    
  { Selection flags  }
    V4L2_SEL_FLAG_GE = 1 shl 0;    
    V4L2_SEL_FLAG_LE = 1 shl 1;    
    V4L2_SEL_FLAG_KEEP_CONFIG = 1 shl 2;    
  { Backward compatibility flag definitions --- to be removed.  }
    V4L2_SUBDEV_SEL_FLAG_SIZE_GE = V4L2_SEL_FLAG_GE;    
    V4L2_SUBDEV_SEL_FLAG_SIZE_LE = V4L2_SEL_FLAG_LE;    
    V4L2_SUBDEV_SEL_FLAG_KEEP_CONFIG = V4L2_SEL_FLAG_KEEP_CONFIG;    
{$endif}
  { __V4L2_COMMON__  }
{//#include <linux/v4l2-mediabus.h>}
  {
   * Media Bus API header
   *
   * Copyright (C) 2009, Guennadi Liakhovetski <g.liakhovetski@gmx.de>
   *
   * This program is free software; you can redistribute it and/or modify
   * it under the terms of the GNU General Public License version 2 as
   * published by the Free Software Foundation.
    }
{$ifndef __LINUX_V4L2_MEDIABUS_H}
{$define __LINUX_V4L2_MEDIABUS_H}  
{//#include <linux/types.h>}
{//#include <linux/videodev2.h>}
  {
   * These pixel codes uniquely identify data formats on the media bus. Mostly
   * they correspond to similarly named V4L2_PIX_FMT_* formats, format 0 is
   * reserved, V4L2_MBUS_FMT_FIXED shall be used by host-client pairs, where the
   * data format is fixed. Additionally, "2X8" means that one pixel is transferred
   * in two 8-bit samples, "BE" or "LE" specify in which order those samples are
   * transferred over the bus: "LE" means that the least significant bits are
   * transferred first, "BE" means that the most significant bits are transferred
   * first, and "PADHI" and "PADLO" define which bits - low or high, in the
   * incomplete high byte, are filled with padding bits.
   *
   * The pixel codes are grouped by type, bus_width, bits per component, samples
   * per pixel and order of subsamples. Numerical values are sorted using generic
   * numerical sort order (8 thus comes before 10).
   *
   * As their value can't change when a new pixel code is inserted in the
   * enumeration, the pixel codes are explicitly given a numerical value. The next
   * free values for each category are listed below, update them when inserting
   * new pixel codes.
    }
  { RGB - next is 0x100e  }
  { YUV (including grey) - next is 0x2018  }
  { Bayer - next is 0x3019  }
  { JPEG compressed formats - next is 0x4002  }
  { Vendor specific formats - next is 0x5002  }
  { S5C73M3 sensor specific interleaved UYVY and JPEG  }

  type
    v4l2_mbus_pixelcode =  Longint;
    Const
      V4L2_MBUS_FMT_FIXED = $0001;
      V4L2_MBUS_FMT_RGB444_2X8_PADHI_BE = $1001;
      V4L2_MBUS_FMT_RGB444_2X8_PADHI_LE = $1002;
      V4L2_MBUS_FMT_RGB555_2X8_PADHI_BE = $1003;
      V4L2_MBUS_FMT_RGB555_2X8_PADHI_LE = $1004;
      V4L2_MBUS_FMT_BGR565_2X8_BE = $1005;
      V4L2_MBUS_FMT_BGR565_2X8_LE = $1006;
      V4L2_MBUS_FMT_RGB565_2X8_BE = $1007;
      V4L2_MBUS_FMT_RGB565_2X8_LE = $1008;
      V4L2_MBUS_FMT_RGB666_1X18 = $1009;
      V4L2_MBUS_FMT_RGB888_1X24 = $100a;
      V4L2_MBUS_FMT_RGB888_2X12_BE = $100b;
      V4L2_MBUS_FMT_RGB888_2X12_LE = $100c;
      V4L2_MBUS_FMT_ARGB8888_1X32 = $100d;
      V4L2_MBUS_FMT_Y8_1X8 = $2001;
      V4L2_MBUS_FMT_UV8_1X8 = $2015;
      V4L2_MBUS_FMT_UYVY8_1_5X8 = $2002;
      V4L2_MBUS_FMT_VYUY8_1_5X8 = $2003;
      V4L2_MBUS_FMT_YUYV8_1_5X8 = $2004;
      V4L2_MBUS_FMT_YVYU8_1_5X8 = $2005;
      V4L2_MBUS_FMT_UYVY8_2X8 = $2006;
      V4L2_MBUS_FMT_VYUY8_2X8 = $2007;
      V4L2_MBUS_FMT_YUYV8_2X8 = $2008;
      V4L2_MBUS_FMT_YVYU8_2X8 = $2009;
      V4L2_MBUS_FMT_Y10_1X10 = $200a;
      V4L2_MBUS_FMT_YUYV10_2X10 = $200b;
      V4L2_MBUS_FMT_YVYU10_2X10 = $200c;
      V4L2_MBUS_FMT_Y12_1X12 = $2013;
      V4L2_MBUS_FMT_UYVY8_1X16 = $200f;
      V4L2_MBUS_FMT_VYUY8_1X16 = $2010;
      V4L2_MBUS_FMT_YUYV8_1X16 = $2011;
      V4L2_MBUS_FMT_YVYU8_1X16 = $2012;
      V4L2_MBUS_FMT_YDYUYDYV8_1X16 = $2014;
      V4L2_MBUS_FMT_YUYV10_1X20 = $200d;
      V4L2_MBUS_FMT_YVYU10_1X20 = $200e;
      V4L2_MBUS_FMT_YUV10_1X30 = $2016;
      V4L2_MBUS_FMT_AYUV8_1X32 = $2017;
      V4L2_MBUS_FMT_SBGGR8_1X8 = $3001;
      V4L2_MBUS_FMT_SGBRG8_1X8 = $3013;
      V4L2_MBUS_FMT_SGRBG8_1X8 = $3002;
      V4L2_MBUS_FMT_SRGGB8_1X8 = $3014;
      V4L2_MBUS_FMT_SBGGR10_ALAW8_1X8 = $3015;
      V4L2_MBUS_FMT_SGBRG10_ALAW8_1X8 = $3016;
      V4L2_MBUS_FMT_SGRBG10_ALAW8_1X8 = $3017;
      V4L2_MBUS_FMT_SRGGB10_ALAW8_1X8 = $3018;
      V4L2_MBUS_FMT_SBGGR10_DPCM8_1X8 = $300b;
      V4L2_MBUS_FMT_SGBRG10_DPCM8_1X8 = $300c;
      V4L2_MBUS_FMT_SGRBG10_DPCM8_1X8 = $3009;
      V4L2_MBUS_FMT_SRGGB10_DPCM8_1X8 = $300d;
      V4L2_MBUS_FMT_SBGGR10_2X8_PADHI_BE = $3003;
      V4L2_MBUS_FMT_SBGGR10_2X8_PADHI_LE = $3004;
      V4L2_MBUS_FMT_SBGGR10_2X8_PADLO_BE = $3005;
      V4L2_MBUS_FMT_SBGGR10_2X8_PADLO_LE = $3006;
      V4L2_MBUS_FMT_SBGGR10_1X10 = $3007;
      V4L2_MBUS_FMT_SGBRG10_1X10 = $300e;
      V4L2_MBUS_FMT_SGRBG10_1X10 = $300a;
      V4L2_MBUS_FMT_SRGGB10_1X10 = $300f;
      V4L2_MBUS_FMT_SBGGR12_1X12 = $3008;
      V4L2_MBUS_FMT_SGBRG12_1X12 = $3010;
      V4L2_MBUS_FMT_SGRBG12_1X12 = $3011;
      V4L2_MBUS_FMT_SRGGB12_1X12 = $3012;
      V4L2_MBUS_FMT_JPEG_1X8 = $4001;
      V4L2_MBUS_FMT_S5C_UYVY_JPEG_1X8 = $5001;

  {*
   * struct v4l2_mbus_framefmt - frame format on the media bus
   * @width:	frame width
   * @height:	frame height
   * @code:	data format code (from enum v4l2_mbus_pixelcode)
   * @field:	used interlacing type (from enum v4l2_field)
   * @colorspace:	colorspace of the data (from enum v4l2_colorspace)
    }

  type
    Pv4l2_mbus_framefmt = ^v4l2_mbus_framefmt;
    v4l2_mbus_framefmt = record
        width : __u32;
        height : __u32;
        code : __u32;
        field : __u32;
        colorspace : __u32;
        reserved : array[0..6] of __u32;
      end;

{$endif}
  // __LINUX_V4L2_MEDIABUS_H  
  {*
   * enum v4l2_subdev_format_whence - Media bus format type
   * @V4L2_SUBDEV_FORMAT_TRY: try format, for negotiation only
   * @V4L2_SUBDEV_FORMAT_ACTIVE: active format, applied to the device
    }

  type
    v4l2_subdev_format_whence =  Longint;
    Const
      V4L2_SUBDEV_FORMAT_TRY = 0;
      V4L2_SUBDEV_FORMAT_ACTIVE = 1;

  {*
   * struct v4l2_subdev_format - Pad-level media bus format
   * @which: format type (from enum v4l2_subdev_format_whence)
   * @pad: pad number, as reported by the media API
   * @format: media bus format (format code and frame size)
    }

  type
    Pv4l2_subdev_format = ^v4l2_subdev_format;
    v4l2_subdev_format = record
        which : __u32;
        pad : __u32;
        format : v4l2_mbus_framefmt;
        reserved : array[0..7] of __u32;
      end;

  {*
   * struct v4l2_subdev_crop - Pad-level crop settings
   * @which: format type (from enum v4l2_subdev_format_whence)
   * @pad: pad number, as reported by the media API
   * @rect: pad crop rectangle boundaries
    }
    Pv4l2_subdev_crop = ^v4l2_subdev_crop;
    v4l2_subdev_crop = record
        which : __u32;
        pad : __u32;
        rect : v4l2_rect;
        reserved : array[0..7] of __u32;
      end;

  {*
   * struct v4l2_subdev_mbus_code_enum - Media bus format enumeration
   * @pad: pad number, as reported by the media API
   * @index: format index during enumeration
   * @code: format code (from enum v4l2_mbus_pixelcode)
    }
    Pv4l2_subdev_mbus_code_enum = ^v4l2_subdev_mbus_code_enum;
    v4l2_subdev_mbus_code_enum = record
        pad : __u32;
        index : __u32;
        code : __u32;
        reserved : array[0..8] of __u32;
      end;

  {*
   * struct v4l2_subdev_frame_size_enum - Media bus format enumeration
   * @pad: pad number, as reported by the media API
   * @index: format index during enumeration
   * @code: format code (from enum v4l2_mbus_pixelcode)
    }
    Pv4l2_subdev_frame_size_enum = ^v4l2_subdev_frame_size_enum;
    v4l2_subdev_frame_size_enum = record
        index : __u32;
        pad : __u32;
        code : __u32;
        min_width : __u32;
        max_width : __u32;
        min_height : __u32;
        max_height : __u32;
        reserved : array[0..8] of __u32;
      end;

  {*
   * struct v4l2_subdev_frame_interval - Pad-level frame rate
   * @pad: pad number, as reported by the media API
   * @interval: frame interval in seconds
    }
    Pv4l2_subdev_frame_interval = ^v4l2_subdev_frame_interval;
    v4l2_subdev_frame_interval = record
        pad : __u32;
        interval : v4l2_fract;
        reserved : array[0..8] of __u32;
      end;

  {*
   * struct v4l2_subdev_frame_interval_enum - Frame interval enumeration
   * @pad: pad number, as reported by the media API
   * @index: frame interval index during enumeration
   * @code: format code (from enum v4l2_mbus_pixelcode)
   * @width: frame width in pixels
   * @height: frame height in pixels
   * @interval: frame interval in seconds
    }
    Pv4l2_subdev_frame_interval_enum = ^v4l2_subdev_frame_interval_enum;
    v4l2_subdev_frame_interval_enum = record
        index : __u32;
        pad : __u32;
        code : __u32;
        width : __u32;
        height : __u32;
        interval : v4l2_fract;
        reserved : array[0..8] of __u32;
      end;

  {*
   * struct v4l2_subdev_selection - selection info
   *
   * @which: either V4L2_SUBDEV_FORMAT_ACTIVE or V4L2_SUBDEV_FORMAT_TRY
   * @pad: pad number, as reported by the media API
   * @target: Selection target, used to choose one of possible rectangles,
   *	    defined in v4l2-common.h; V4L2_SEL_TGT_* .
   * @flags: constraint flags, defined in v4l2-common.h; V4L2_SEL_FLAG_*.
   * @r: coordinates of the selection window
   * @reserved: for future use, set to zero for now
   *
   * Hardware may use multiple helper windows to process a video stream.
   * The structure is used to exchange this selection areas between
   * an application and a driver.
    }
    Pv4l2_subdev_selection = ^v4l2_subdev_selection;
    v4l2_subdev_selection = record
        which : __u32;
        pad : __u32;
        target : __u32;
        flags : __u32;
        r : v4l2_rect;
        reserved : array[0..7] of __u32;
      end;

  {__user }    Pv4l2_subdev_edid = ^v4l2_subdev_edid;
    v4l2_subdev_edid = record
        pad : __u32;
        start_block : __u32;
        blocks : __u32;
        reserved : array[0..4] of __u32;
        edid : ^__u8;
      end;

const
  VIDIOC_SUBDEV_G_FMT     = LongInt(((_IOC_READ or _IOC_WRITE) shl _IOC_DIRSHIFT) or (Ord('V') shl _IOC_TYPESHIFT) or (4 shl _IOC_NRSHIFT) or (SizeOf(v4l2_subdev_format) shl _IOC_SIZESHIFT));
  VIDIOC_SUBDEV_S_FMT     = LongInt(((_IOC_READ or _IOC_WRITE) shl _IOC_DIRSHIFT) or (Ord('V') shl _IOC_TYPESHIFT) or (5 shl _IOC_NRSHIFT) or (SizeOf(v4l2_subdev_format) shl _IOC_SIZESHIFT));
  VIDIOC_SUBDEV_G_FRAME_INTERVAL = LongInt(((_IOC_READ or _IOC_WRITE) shl _IOC_DIRSHIFT) or (Ord('V') shl _IOC_TYPESHIFT) or (21 shl _IOC_NRSHIFT) or (SizeOf(v4l2_subdev_frame_interval) shl _IOC_SIZESHIFT));
  VIDIOC_SUBDEV_S_FRAME_INTERVAL = LongInt(((_IOC_READ or _IOC_WRITE) shl _IOC_DIRSHIFT) or (Ord('V') shl _IOC_TYPESHIFT) or (22 shl _IOC_NRSHIFT) or (SizeOf(v4l2_subdev_frame_interval) shl _IOC_SIZESHIFT));
  VIDIOC_SUBDEV_ENUM_MBUS_CODE = LongInt(((_IOC_READ or _IOC_WRITE) shl _IOC_DIRSHIFT) or (Ord('V') shl _IOC_TYPESHIFT) or (2 shl _IOC_NRSHIFT) or (SizeOf(v4l2_subdev_mbus_code_enum) shl _IOC_SIZESHIFT));
  VIDIOC_SUBDEV_ENUM_FRAME_SIZE = LongInt(((_IOC_READ or _IOC_WRITE) shl _IOC_DIRSHIFT) or (Ord('V') shl _IOC_TYPESHIFT) or (74 shl _IOC_NRSHIFT) or (SizeOf(v4l2_subdev_frame_size_enum) shl _IOC_SIZESHIFT));
  VIDIOC_SUBDEV_ENUM_FRAME_INTERVAL = LongInt(((_IOC_READ or _IOC_WRITE) shl _IOC_DIRSHIFT) or (Ord('V') shl _IOC_TYPESHIFT) or (75 shl _IOC_NRSHIFT) or (SizeOf(v4l2_subdev_frame_interval_enum) shl _IOC_SIZESHIFT));
  VIDIOC_SUBDEV_G_CROP    = LongInt(((_IOC_READ or _IOC_WRITE) shl _IOC_DIRSHIFT) or (Ord('V') shl _IOC_TYPESHIFT) or (59 shl _IOC_NRSHIFT) or (SizeOf(v4l2_subdev_crop) shl _IOC_SIZESHIFT));
  VIDIOC_SUBDEV_S_CROP    = LongInt(((_IOC_READ or _IOC_WRITE) shl _IOC_DIRSHIFT) or (Ord('V') shl _IOC_TYPESHIFT) or (60 shl _IOC_NRSHIFT) or (SizeOf(v4l2_subdev_crop) shl _IOC_SIZESHIFT));
  VIDIOC_SUBDEV_G_SELECTION = LongInt(((_IOC_READ or _IOC_WRITE) shl _IOC_DIRSHIFT) or (Ord('V') shl _IOC_TYPESHIFT) or (61 shl _IOC_NRSHIFT) or (SizeOf(v4l2_subdev_selection) shl _IOC_SIZESHIFT));
  VIDIOC_SUBDEV_S_SELECTION = LongInt(((_IOC_READ or _IOC_WRITE) shl _IOC_DIRSHIFT) or (Ord('V') shl _IOC_TYPESHIFT) or (62 shl _IOC_NRSHIFT) or (SizeOf(v4l2_subdev_selection) shl _IOC_SIZESHIFT));
  VIDIOC_SUBDEV_G_EDID    = LongInt(((_IOC_READ or _IOC_WRITE) shl _IOC_DIRSHIFT) or (Ord('V') shl _IOC_TYPESHIFT) or (40 shl _IOC_NRSHIFT) or (SizeOf(v4l2_subdev_edid) shl _IOC_SIZESHIFT));
  VIDIOC_SUBDEV_S_EDID    = LongInt(((_IOC_READ or _IOC_WRITE) shl _IOC_DIRSHIFT) or (Ord('V') shl _IOC_TYPESHIFT) or (41 shl _IOC_NRSHIFT) or (SizeOf(v4l2_subdev_edid) shl _IOC_SIZESHIFT));

{$endif}

implementation

end.
