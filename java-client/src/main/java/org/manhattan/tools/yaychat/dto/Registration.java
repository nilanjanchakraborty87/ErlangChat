package org.manhattan.tools.yaychat.dto;

import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonPropertyOrder;

@JsonInclude(JsonInclude.Include.NON_EMPTY)
@JsonPropertyOrder({"name", "mobile", "email", "password"})
public class Registration{
    private String fname;
    private String lname;
    private long mobile;
    private String email;
    private String password;

	public String getFname() {
		return fname;
	}



	public void setFname(String fname) {
		this.fname = fname;
	}



	public String getLname() {
		return lname;
	}



	public void setLname(String lname) {
		this.lname = lname;
	}



	public long getMobile() {
		return mobile;
	}

	public void setMobile(long mobile) {
		this.mobile = mobile;
	}

	public String getEmail() {
		return email;
	}

	public void setEmail(String email) {
		this.email = email;
	}

	public String getPassword() {
		return password;
	}

	public void setPassword(String password) {
		this.password = password;
	}

	public Registration(String fname, String lname, long mobile, String email, String password) {
		super();
		this.fname = fname;
		this.lname = lname;
		this.mobile = mobile;
		this.email = email;
		this.password = password;
	}



	public Registration() {
		super();
		// TODO Auto-generated constructor stub
	}
    
    
}
