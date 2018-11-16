package org.manhattan.tools.yaychat.dto;


import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.NoArgsConstructor;

@JsonInclude(JsonInclude.Include.NON_EMPTY)
@JsonIgnoreProperties({"recordType"})
public class ChatDetail extends Record{
    @JsonProperty("__rec")
    final String __rec = "chat_detail";
    private long from;
    private long to;
    private String message;
    
    public ChatDetail() {
    	
    }
    
    public ChatDetail(long from, long to, String message) {
		super();
		this.from = from;
		this.to = to;
		this.message = message;
	}



	public long getFrom() {
		return from;
	}



	public void setFrom(long from) {
		this.from = from;
	}



	public long getTo() {
		return to;
	}



	public void setTo(long to) {
		this.to = to;
	}



	public String getMessage() {
		return message;
	}



	public void setMessage(String message) {
		this.message = message;
	}



	@Override
    public String getRecordType() {
        return "chat";
    }
}
