/*<copyright>
 * <year>1999-2007</year>
 * <holder>Ericsson AB, All Rights Reserved</holder>
 *</copyright>
 *<legalnotice>
 * The contents of this file are subject to the Erlang Public License,
 * Version 1.1, (the "License"); you may not use this file except in
 * compliance with the License. You should have received a copy of the
 * Erlang Public License along with this software. If not, it can be
 * retrieved online at http://www.erlang.org/.
 *
 * Software distributed under the License is distributed on an "AS IS"
 * basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
 * the License for the specific language governing rights and limitations
 * under the License.
 *
 * The Initial Developer of the Original Code is Ericsson AB.
 *</legalnotice>
 */
package com.ericsson.otp.ic;

/**
  Helper class for Pid.
  **/

public class PidHelper {

   // constructors
   private PidHelper() {}

   // methods
   /**
    Marshal method for the Pid class, encodes the Pid object to the output stream.
    **/
   public static void marshal(com.ericsson.otp.erlang.OtpOutputStream _out, Pid _value)
     throws java.lang.Exception {

       _out.write_pid(_value.node(),_value.id(),_value.serial(),_value.creation());
   }

   /**
    Unmarshal method for the Pid class, decodes a Pid object from the stream.
    @return Pid, read from the input stream
    **/
   public static Pid unmarshal(com.ericsson.otp.erlang.OtpInputStream _in)
     throws java.lang.Exception {

       // Double job is done here, there should be 
       // a function returning a Pid instead of an
       // OtpErlangPid
       com.ericsson.otp.erlang.OtpErlangPid oep = _in.read_pid(); 

       return new Pid(oep.node(),oep.id(),oep.serial(),oep.creation());      
   }

   /**
    Standard method that returns the interface repository identity.
    @return String containing the interface repository identity of Pid
    **/
   public static String id() {
      return "IDL:com/ericsson/otp/ic/Pid:1.0";
   }

   /**
    Standard method that returns the Pid class name.
    @return String containing the class name of Pid
    **/
   public static String name() {
      return "Pid";
   }

   /**
    Holds the TypeCode
    **/
   private static com.ericsson.otp.ic.TypeCode _tc;

   /**
     Standard TypeCode accessor method.
     @return the TypeCode for Pid
     **/
   synchronized public static com.ericsson.otp.ic.TypeCode type() {

     if (_tc != null)
       return _tc;

     com.ericsson.otp.ic.TypeCode _tc0 =
       new com.ericsson.otp.ic.TypeCode();
     _tc0.kind(com.ericsson.otp.ic.TCKind.tk_struct);
     _tc0.id("IDL:com/ericsson/otp/ic/Pid:1.0");
     _tc0.name("Pid");
     _tc0.member_count(4);
     _tc0.member_name(0,"node");
     com.ericsson.otp.ic.TypeCode _tc1 =
       new com.ericsson.otp.ic.TypeCode();
     _tc1.kind(com.ericsson.otp.ic.TCKind.tk_string);
     _tc1.length(256);
     _tc0.member_type(0,_tc1);
     _tc0.member_name(1,"num");
     com.ericsson.otp.ic.TypeCode _tc2 =
       new com.ericsson.otp.ic.TypeCode();
     _tc2.kind(com.ericsson.otp.ic.TCKind.tk_ulong);
     _tc0.member_type(1,_tc2);
     _tc0.member_name(2,"serial");
     com.ericsson.otp.ic.TypeCode _tc3 =
       new com.ericsson.otp.ic.TypeCode();
     _tc3.kind(com.ericsson.otp.ic.TCKind.tk_ulong);
     _tc0.member_type(2,_tc3);
     _tc0.member_name(3,"creation");
     com.ericsson.otp.ic.TypeCode _tc4 =
       new com.ericsson.otp.ic.TypeCode();
     _tc4.kind(com.ericsson.otp.ic.TCKind.tk_ulong);
     _tc0.member_type(3,_tc4);

     _tc = _tc0;

     return _tc0;
   }


  /**
    Standard method for inserting a Pid to an Any.
    **/
  public static void insert(com.ericsson.otp.ic.Any _any, Pid _this)
    throws java.lang.Exception {
      
      com.ericsson.otp.erlang.OtpOutputStream _os = 
	new com.ericsson.otp.erlang.OtpOutputStream();
      
      _any.type(type());
      marshal(_os, _this);
      _any.insert_Streamable(_os);
  }
  
  /**
    Standard method for extracting a Pid from an Any.
    @return Pid, the value found in an Any contained stream.
    **/
   public static Pid extract(com.ericsson.otp.ic.Any _any)
     throws java.lang.Exception {

     return unmarshal(_any.extract_Streamable());
   }

}
