package org.manhattan.tools.yaychat.dto;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

public class Request<T> {
    private String type;
    private T data;
    
    
	public String getType() {
		return type;
	}
	public void setType(String type) {
		this.type = type;
	}
	public T getData() {
		return data;
	}
	public void setData(T data) {
		this.data = data;
	}
	
	
	public Request(String type, T data) {
		super();
		this.type = type;
		this.data = data;
	}
	public Request() {
		super();
		// TODO Auto-generated constructor stub
	}
    
	
	
	
    
}
