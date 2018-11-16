package org.manhattan.tools.yaychat.dto;

import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@JsonInclude(JsonInclude.Include.NON_EMPTY)
public class Response<E> {

    @JsonProperty("isSuccess")
    private String success;
    private String message;
    private E data;
    private String type;
    
    
	public String getSuccess() {
		return success;
	}
	public void setSuccess(String success) {
		this.success = success;
	}
	public String getMessage() {
		return message;
	}
	public void setMessage(String message) {
		this.message = message;
	}
	public E getData() {
		return data;
	}
	public void setData(E data) {
		this.data = data;
	}
	public String getType() {
		return type;
	}
	public void setType(String type) {
		this.type = type;
	}
	
	public Response() {
		super();
		// TODO Auto-generated constructor stub
	}
	
	public Response(String success, String message, E data, String type) {
		super();
		this.success = success;
		this.message = message;
		this.data = data;
		this.type = type;
	}
    
	
	
    
}
