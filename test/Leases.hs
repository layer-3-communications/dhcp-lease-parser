{-# language QuasiQuotes #-}

module Leases where

import Data.ByteString.Lazy (ByteString)
import NeatInterpolation
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString.Builder as BB

tb :: Text -> ByteString
tb = BB.toLazyByteString . TE.encodeUtf8Builder

lease0,lease1,lease2,lease3,lease4,lease5,lease6,lease7 :: ByteString
lease0 = tb $
  [text|
  lease 10.160.23.253 {
    starts 1 2016/01/11 18:31:21;
    ends 1 2016/02/08 18:38:01;
    tstp 1 2016/02/08 18:38:01;
    binding state free;
    hardware ethernet 00:02:80:05:3e:9a;
  }
  |]
lease1 = tb $
  [text|
  lease 10.152.33.140 {
    starts 4 2017/05/18 14:05:18;
    ends 4 2017/06/15 14:11:58;
    binding state active;
    next binding state free;
    hardware ethernet 00:02:80:04:30:63;
    uid "";
    client-hostname "000280043063";
  }
  |]
lease2 = tb $
  [text|
  lease 10.102.18.226 {
    starts 4 2017/05/18 05:40:43;
    ends 5 2017/05/19 00:40:43;
    binding state active;
    next binding state free;
    hardware ethernet 00:02:a1:23:03:80;
    uid "";
  }
  |]
lease3 = tb $
  [text|
  lease 10.160.22.231 {
    starts 3 2018/02/21 23:51:28;
    ends 3 2018/03/21 23:58:08;
    binding state active;
    next binding state free;
    hardware ethernet 00:02:80:04:34:e8;
    uid "";
    client-hostname "0002800434E8";
  }
  |]
lease4 = tb $
  [text|
  lease 10.160.22.225 {
    starts 3 2018/02/21 23:52:59;
    ends 3 2018/03/21 23:59:39;
    binding state active;
    next binding state free;
    hardware ethernet 00:02:80:04:3b:39;
    uid "";
    client-hostname "000280043B39";
  }
  |]
lease5 = tb $
  [text|
  lease 10.160.22.227 {
    starts 1 2018/02/26 17:18:08;
    ends 1 2018/03/26 17:24:48;
    binding state active;
    next binding state free;
    hardware ethernet 00:02:80:04:1d:7f;
    uid "";
    client-hostname "000280041D7F";
  }
  |]
lease6 = tb $
  [text|
  lease 10.160.22.226 {
    starts 1 2018/02/26 17:19:27;
    ends 1 2018/03/26 17:26:07;
    binding state active;
    next binding state free;
    hardware ethernet 00:02:80:04:22:48;
    uid "\001\000\002\200\004\"H";
    client-hostname "000280042248";
  }
  |]
lease7 = tb $
  [text|
  # 00:02:80:04;

  lease 10.153.2.77 {
    starts 3 2015/12/30 08:48:24;
    ends 3 2015/12/30 08:50:24;
    tstp 3 2015/12/30 08:50:24;
    binding state free;
    hardware ethernet 00:00:00:00:00:00;
    uid "";
    client-hostname "000280045079";
  }
  |]
lease8 = tb $
  [text|
  lease 10.153.2.77 {
    starts 3 2015/12/30 08:48:24;
    ends 3 2015/12/30 08:50:24;
    tstp 3 2015/12/30 08:50:24;
    binding state free;
    hardware ethernet 00:00:00:00:00:00;
    uid "";
    client-hostname "000280045079";
  }
  lease 10.153.2.77 {
    starts 3 2015/12/30 08:48:24;
    ends 3 2015/12/30 08:50:24;
    tstp 3 2015/12/30 08:50:24;
    binding state free;
    hardware ethernet 00:00:00:00:00:00;
    uid "";
    client-hostname "000280045079";
  }
  |]
lease9 = tb $
  [text|
  lease 10.153.2.77 {
    starts 3 2015/12/30 08:48:24;
    ends 3 2015/12/30 08:50:24;
    tstp 3 2015/12/30 08:50:24;
    binding state free;
    hardware ethernet 00:00:00:00;
    uid "";
    client-hostname "000280045079";
  }
  lease 10.153.2.77 {
    starts 3 2015/12/30 08:48:24;
    ends 3 2015/12/30 08:50:24;
    tstp 3 2015/12/30 08:50:24;
    binding state free;
    hardware ethernet 00:00:00:00:00:00;
    uid "";
    client-hostname "000280045079";
  }
  |]


