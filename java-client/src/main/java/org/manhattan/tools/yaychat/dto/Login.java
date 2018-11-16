package org.manhattan.tools.yaychat.dto;

import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.*;

@JsonInclude(JsonInclude.Include.NON_EMPTY)
public class Login extends Record{
    @JsonProperty("__rec")
    final String __rec = "login";
    private long mobile;
    private String password;
    
    public Login() {
		super();
		// TODO Auto-generated constructor stub
	}



	public Login(long mobile, String password) {
		super();
		this.mobile = mobile;
		this.password = password;
	}



	public long getMobile() {
		return mobile;
	}



	public void setMobile(long mobile) {
		this.mobile = mobile;
	}



	public String getPassword() {
		return password;
	}



	public void setPassword(String password) {
		this.password = password;
	}



	@Override
    @JsonProperty("__rec")
    public String getRecordType() {
        return __rec;
    }
}
